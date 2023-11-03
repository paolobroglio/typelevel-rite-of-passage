package com.paolobroglio.projects.jobsboard.http.routes

import cats.effect.*
import cats.implicits.*
import com.paolobroglio.projects.jobsboard.core.Auth
import com.paolobroglio.projects.jobsboard.domain.auth.{LoginInfo, NewPasswordInfo}
import com.paolobroglio.projects.jobsboard.domain.error
import com.paolobroglio.projects.jobsboard.domain.error.AppError
import com.paolobroglio.projects.jobsboard.domain.security.*
import com.paolobroglio.projects.jobsboard.domain.user.{NewUserInfo, User}
import com.paolobroglio.projects.jobsboard.http.responses.FailureResponse
import com.paolobroglio.projects.jobsboard.http.validation.syntax.*
import com.paolobroglio.projects.jobsboard.logging.syntax.*
import io.circe.generic.auto.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.headers.Authorization
import org.http4s.server.Router
import org.http4s.{Header, Headers, HttpRoutes, Request, Response, Status}
import org.typelevel.ci.CIStringSyntax
import org.typelevel.log4cats.Logger
import tsec.authentication.{JWTAuthenticator, SecuredRequestHandler, TSecAuthService, asAuthed}
class AuthRoutes[F[_]: Concurrent: Logger: SecuredHandler] private (auth: Auth[F], authenticator: Authenticator[F]) extends HttpValidationDsl[F] {

  private def manageAppError(appError: AppError): F[Response[F]] =
    appError match {
      case error.WrongPasswordError(message) =>
        Response(Status.Unauthorized).pure[F]
      case error.UserNotFoundError(message) =>
        NotFound(FailureResponse(message))
      case error.UserCreationError(message) =>
        BadRequest(FailureResponse(message))
    }

  private val loginRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case req@POST -> Root / "login" =>
      req.validate[LoginInfo] { loginInfo =>
        val maybeJwtToken = for {
          _ <- Logger[F].info("Login called")
          maybeUser <- auth.login(loginInfo.email, loginInfo.password).logError(e => s"Failed login user: $e")
          _ <- Logger[F].debug("Login done, creating token")
          maybeToken <- maybeUser.traverse(user => authenticator.create(user.email))
        } yield maybeToken

        maybeJwtToken.map {
          case Right(jwt) => authenticator.embed(Response(Status.Ok), jwt)
          case Left(value) => Response(Status.Unauthorized)
        }
      }
  }

  private val logoutRoute: AuthRoute[F] = {
    case req@POST -> Root / "logout" asAuthed user =>
      for {
        _ <- Logger[F].info("logout called")
        resp <- auth.logout(user.email).logError(e => s"Failed logout user: $e").flatMap {
          case Right(_) =>
            Ok()
          case Left(appError) =>
            manageAppError(appError)
        }
      } yield resp
  }

  private val signUpRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case req@POST -> Root / "signUp" =>
      req.validate[NewUserInfo] { newUserInfo =>
        for {
          _ <- Logger[F].info("signUp called")
          resp <- auth.signUp(newUserInfo).logError(e => s"Failed signing up user: $e").flatMap {
            case Right(createdUser) =>
              Created()
            case Left(appError) =>
              manageAppError(appError)
          }
        } yield resp
      }
  }

  private val changePasswordRoute: AuthRoute[F] = {
    case req@PUT -> Root / "password" asAuthed user =>
      for {
        newPasswordInfo <- req.request.as[NewPasswordInfo].logError(e => "Failed to parse request: $e")
        _ <- Logger[F].info("changePassword called")
        resp <- auth.changePassword(user.email, newPasswordInfo).logError(e => s"Failed changing password for user: $e").flatMap {
          case Right(user) =>
            NoContent()
          case Left(appError) =>
            manageAppError(appError)
        }
      } yield resp
  }

  private val deleteUserRoute: AuthRoute[F] = {
    case req@DELETE -> Root / "user" / id asAuthed user =>
      for {
        resp <- NoContent()
      } yield resp
  }

  val nonAuthedRoutes = loginRoute <+> signUpRoute
  val authedRoutes = SecuredHandler[F].liftService(
    changePasswordRoute.restrictedTo(allRoles) |+|
      logoutRoute.restrictedTo(allRoles) |+|
      deleteUserRoute.restrictedTo(adminOnly)
  )

  val routes: HttpRoutes[F] = Router(
    "/auth" -> (nonAuthedRoutes <+> authedRoutes)
  )
}

object AuthRoutes {
  def apply[F[_]: Concurrent: Logger: SecuredHandler] (auth: Auth[F], authenticator: Authenticator[F]) =
    new AuthRoutes[F](auth, authenticator)
}