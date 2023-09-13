package com.paolobroglio.projects.jobsboard.http.routes

import cats.data.OptionT
import cats.effect.*
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.implicits.*
import com.paolobroglio.projects.jobsboard.core.{Auth, Jobs, Users}
import com.paolobroglio.projects.jobsboard.domain.auth.{LoginInfo, NewPasswordInfo}
import com.paolobroglio.projects.jobsboard.domain.error.{AppError, UserCreationError, UserNotFoundError, WrongPasswordError}
import com.paolobroglio.projects.jobsboard.domain.job.{Job, JobFilter}
import com.paolobroglio.projects.jobsboard.domain.pagination.Pagination
import com.paolobroglio.projects.jobsboard.domain.security.{Authenticator, JwtToken}
import com.paolobroglio.projects.jobsboard.domain.user.{NewUserInfo, User}
import com.paolobroglio.projects.jobsboard.domain.{auth, job, user}
import com.paolobroglio.projects.jobsboard.fixtures.{JobFixture, UserFixture}
import io.circe.generic.auto.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.Authorization
import org.http4s.implicits.uri
import org.http4s.{AuthScheme, Credentials, HttpRoutes, Method, Request, Status, Uri}
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.shouldBe
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import tsec.authentication.{IdentityStore, JWTAuthenticator}
import tsec.jws.mac.JWTMac
import tsec.mac.jca.HMACSHA256
import tsec.passwordhashers.PasswordHash
import tsec.passwordhashers.jca.BCrypt

import scala.concurrent.duration.DurationInt

class AuthRoutesSpec extends AsyncFreeSpec
  with AsyncIOSpec
  with Matchers
  with Http4sDsl[IO]
  with UserFixture {

  val mockedAuthenticator: Authenticator[IO] = {
    val key = HMACSHA256.unsafeGenerateKey
    val idStore: IdentityStore[IO, String, User] = (email: String) =>
      if (email == AverageJoe.email) OptionT.pure(AverageJoe)
      else if (email == JohnDoe.email) OptionT.pure(JohnDoe)
      else OptionT.none[IO, User]

    JWTAuthenticator.unbacked.inBearerToken(
      1.day,
      None,
      idStore,
      key
    )
  }

  val mockedAuth: Auth[IO] = new Auth[IO]:
    override def login(email: String, password: String): IO[Either[AppError, JwtToken]] =
      if (email == AverageJoe.email)
        for {
          checkPassword <- BCrypt.checkpwBool[IO](
            password,
            PasswordHash[BCrypt](AverageJoe.hashedPassword)
          )
          maybeJwt <-
            if (checkPassword)
              mockedAuthenticator.create(email)
                .map(jwt => Right(jwt))
            else
              Left(WrongPasswordError("wrong password")).pure[IO]
        } yield maybeJwt
      else Left(UserNotFoundError("user not found")).pure[IO]

    override def signUp(newUserInfo: NewUserInfo): IO[Either[AppError, User]] =
      if (newUserInfo.email != AverageJoe.email)
        for {
          hashedPassword <- BCrypt.hashpw[IO](newUserInfo.password)
          newUser <- User(1L, newUserInfo.email, hashedPassword, None, None, None, user.Role.RECRUITER).pure[IO]
        } yield Right(newUser)
      else Left(UserCreationError("email already used")).pure[IO]


    override def authenticator: Authenticator[IO] = mockedAuthenticator

    override def logout(email: String): IO[Either[AppError, User]] =
      if (email == AverageJoe.email)
        Right(AverageJoe).pure[IO]
      else Left(UserNotFoundError("user not found")).pure[IO]

    override def changePassword(email: String, newPasswordInfo: NewPasswordInfo): IO[Either[AppError, User]] =
      if (email == AverageJoe.email)
        for {
          checkPassword <- BCrypt.checkpwBool[IO](
            newPasswordInfo.oldPassword,
            PasswordHash[BCrypt](AverageJoe.hashedPassword)
          )
          maybeUser <-
            if (checkPassword)
              Right(AverageJoe).pure[IO]
            else
              Left(WrongPasswordError("wrong password")).pure[IO]
        } yield maybeUser
      else Left(UserNotFoundError("non existent user")).pure[IO]


  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  val authRoutes: HttpRoutes[IO] = AuthRoutes[IO](mockedAuth).routes

  extension (r: Request[IO])
    def withBearerToken(a: JwtToken): Request[IO] =
      r.putHeaders {
        val jwtString = JWTMac.toEncodedString[IO, HMACSHA256](a.jwt)

        Authorization(Credentials.Token(AuthScheme.Bearer, jwtString))
      }

  "AuthRoutes" - {
    "should login a new user" in {
      for {
        response <- authRoutes.orNotFound.run(
          Request(method = Method.POST, uri = uri"/auth/login")
            .withEntity(LoginInfo(AverageJoe.email, "password1!"))
        )
      } yield {
        response.status shouldBe Status.Ok
        response.headers.get(Authorization.name).isDefined shouldBe true
      }
    }
    "login fails for wrong password" in {
      for {
        response <- authRoutes.orNotFound.run(
          Request(method = Method.POST, uri = uri"/auth/login")
            .withEntity(LoginInfo(AverageJoe.email, "password2!"))
        )
      } yield {
        response.status shouldBe Status.Unauthorized
      }
    }
    "login fails for absent user" in {
      for {
        response <- authRoutes.orNotFound.run(
          Request(method = Method.POST, uri = uri"/auth/login")
            .withEntity(LoginInfo("absent@acme.org", "password1!"))
        )
      } yield {
        response.status shouldBe Status.NotFound
      }
    }
    "signUp creates a new user" in {
      for {
        response <- authRoutes.orNotFound.run(
          Request(method = Method.POST, uri = uri"/auth/signUp")
            .withEntity(NewUserInfo("newuser@email.org", "password1!", None, None, None))
        )
      } yield {
        response.status shouldBe Status.Created
      }
    }
    "signUp fails if email already exist" in {
      for {
        response <- authRoutes.orNotFound.run(
          Request(method = Method.POST, uri = uri"/auth/signUp")
            .withEntity(NewUserInfo(AverageJoe.email, "password1!", None, None, None))
        )
      } yield {
        response.status shouldBe Status.BadRequest
      }
    }
    "logout succeeds" in {
      for {
        jwt <- mockedAuthenticator.create(AverageJoe.email)
        response <- authRoutes.orNotFound.run(
          Request(method = Method.POST, uri = uri"/auth/logout")
            .withBearerToken(jwt)
        )
      } yield {
        response.status shouldBe Status.Ok
      }
    }
    "logout fails if token isn't in the request" in {
      for {
        response <- authRoutes.orNotFound.run(
          Request(method = Method.POST, uri = uri"/auth/logout")
        )
      } yield {
        response.status shouldBe Status.Unauthorized
      }
    }
    "changePassword succeeds" in {
      for {
        jwt <- mockedAuthenticator.create(AverageJoe.email)
        response <- authRoutes.orNotFound.run(
          Request(method = Method.PUT, uri = uri"/auth/password")
            .withBearerToken(jwt)
            .withEntity(NewPasswordInfo("password1!", "password2!"))
        )
      } yield {
        response.status shouldBe Status.NoContent
      }
    }
    "changePassword fails if oldPassword is wrong" in {
      for {
        jwt <- mockedAuthenticator.create(AverageJoe.email)
        response <- authRoutes.orNotFound.run(
          Request(method = Method.PUT, uri = uri"/auth/password")
            .withBearerToken(jwt)
            .withEntity(NewPasswordInfo("wrong-password", "password2!"))
        )
      } yield {
        response.status shouldBe Status.Unauthorized
      }
    }
    "changePassword fails if token is absent" in {
      for {
        response <- authRoutes.orNotFound.run(
          Request(method = Method.PUT, uri = uri"/auth/password")
            .withEntity(NewPasswordInfo("password1!", "password2!"))
        )
      } yield {
        response.status shouldBe Status.Unauthorized
      }
    }
  }

}
