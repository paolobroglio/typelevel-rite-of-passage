package com.paolobroglio.projects.jobsboard.core

import cats.data.OptionT
import cats.effect.*
import cats.effect.implicits.*
import cats.effect.testing.scalatest.AsyncIOSpec
import com.paolobroglio.projects.jobsboard.domain.auth.NewPasswordInfo
import com.paolobroglio.projects.jobsboard.domain.security.{Authenticator, JwtToken}
import com.paolobroglio.projects.jobsboard.domain.user.*
import com.paolobroglio.projects.jobsboard.fixtures.UserFixture
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import tsec.authentication.{IdentityStore, JWTAuthenticator}
import tsec.mac.jca.HMACSHA256

import concurrent.duration.DurationInt
class AuthSpec
  extends AsyncFreeSpec
    with AsyncIOSpec
    with Matchers
    with UserFixture {

  private val mockedUsers: Users[IO] = new Users[IO]:
    override def find(id: Long): IO[Option[User]] =
      if (id == MarioRossi.id) IO.pure(Some(MarioRossi))
      else if (id == 0L) IO.pure(Some(AverageJoe))
      else IO.pure(None)

    override def create(user: User): IO[Either[String, Long]] = IO.pure(Right(user.id))

    override def update(id: Long, user: User): IO[Option[User]] = IO.pure(Some(user))

    override def delete(id: Long): IO[Boolean] = IO.pure(true)

    override def findByEmail(email: String): IO[Option[User]] =
      if (email == MarioRossi.email) IO.pure(Some(MarioRossi))
      else if (email == JohnDoe.email) IO.pure(Some(JohnDoe))
      else IO.pure(None)

  val mockedAuthenticator: Authenticator[IO] = {
    val key = HMACSHA256.unsafeGenerateKey
    val idStore: IdentityStore[IO, String, User] = (email: String) =>
      if (email == MarioRossi.email) OptionT.pure(MarioRossi)
      else if (email == JohnDoe.email) OptionT.pure(JohnDoe)
      else OptionT.none[IO, User]

    JWTAuthenticator.unbacked.inBearerToken(
      1.day,
      None,
      idStore,
      key
    )
  }

  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  "Auth" - {
    "login return no token if user doesn't exist" in {
      val program = for {
        auth <- LiveAuth[IO](mockedUsers, mockedAuthenticator)
        maybeToken <- auth.login("notfound@company.com", "password1!")
      } yield maybeToken

      program.asserting(_ shouldBe None)
    }
    "login return no token if password is wrong" in {
      val program = for {
        auth <- LiveAuth[IO](mockedUsers, mockedAuthenticator)
        maybeToken <- auth.login("mario@company.com", "wrong-password")
      } yield maybeToken

      program.asserting(_ shouldBe None)
    }
    "login return token if succeeds" in {
      val program = for {
        auth <- LiveAuth[IO](mockedUsers, mockedAuthenticator)
        maybeToken <- auth.login("mario@company.com", "password1!")
      } yield maybeToken

      program.asserting(_ shouldBe a [Some[JwtToken]])
    }
    "signUp registers a new user" in {
      val program = for {
        auth <- LiveAuth[IO](mockedUsers, mockedAuthenticator)
        newUser <- auth.signUp(AverageJoeSignUp)
      } yield newUser

      program.asserting(_ shouldBe a [Some[User]])
    }
    "signUp returns None if user already exists" in {
      val program = for {
        auth <- LiveAuth[IO](mockedUsers, mockedAuthenticator)
        newUser <- auth.signUp(JohnDoeSignUp)
      } yield newUser

      program.asserting(_ shouldBe None)
    }
    "changePassword returns updated users with new password" in {
      val program = for {
        auth <- LiveAuth[IO](mockedUsers, mockedAuthenticator)
        result <- auth.changePassword(MarioRossi.email, NewPasswordInfo("password1!", "password2!"))
      } yield result

      program.asserting {
        case Left(_) => fail()
        case Right(maybeUser) => maybeUser shouldBe a [Some[User]]
      }
    }
    "changePassword returns None if user doesn't exist" in {
      val program = for {
        auth <- LiveAuth[IO](mockedUsers, mockedAuthenticator)
        result <- auth.changePassword("absent@acme.com", NewPasswordInfo("password1!", "password2!"))
      } yield result

      program.asserting {
        case Left(_) => fail()
        case Right(maybeUser) => maybeUser shouldBe None
      }
    }
    "changePassword returns Left if user doesn't exist" in {
      val program = for {
        auth <- LiveAuth[IO](mockedUsers, mockedAuthenticator)
        result <- auth.changePassword(MarioRossi.email, NewPasswordInfo("wrong-password", "password2!"))
      } yield result

      program.asserting {
        case Left(error) => error shouldBe "Invalid Password"
        case Right(_) => fail()
      }
    }
  }

}
