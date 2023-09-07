package com.paolobroglio.projects.jobsboard.core

import cats.*
import cats.effect.*
import cats.effect.kernel.MonadCancelThrow
import cats.implicits.*
import com.paolobroglio.projects.jobsboard.domain.auth.NewPasswordInfo
import com.paolobroglio.projects.jobsboard.domain.security.*
import com.paolobroglio.projects.jobsboard.domain.user.*
import org.typelevel.log4cats.Logger
import tsec.authentication.{AugmentedJWT, JWTAuthenticator}
import tsec.mac.jca.HMACSHA256
import tsec.passwordhashers.PasswordHash
import tsec.passwordhashers.jca.BCrypt

trait Auth[F[_]] {
  def login(email: String, password: String): F[Option[JwtToken]]
  def signUp(newUserInfo: NewUserInfo): F[Option[User]]
  def changePassword(email: String, newPasswordInfo: NewPasswordInfo): F[Either[String, Option[User]]]
}

class LiveAuth[F[_]: Async: Logger] private (users: Users[F], authenticator: Authenticator[F]) extends Auth[F] {
  override def login(email: String, password: String): F[Option[JwtToken]] =
    for {
      maybeUser <- users.findByEmail(email)
      maybeValidatedUser <- maybeUser.filterA(user =>
        BCrypt.checkpwBool[F](
          password,
          PasswordHash[BCrypt](user.hashedPassword)
        ))
      maybeJwtToken <- maybeValidatedUser.traverse(user => authenticator.create(user.email))
    } yield maybeJwtToken

  override def signUp(newUserInfo: NewUserInfo): F[Option[User]] =
    users.findByEmail(newUserInfo.email).flatMap {
      case Some(err) =>
        None.pure[F]
      case None =>
        for {
          hashedPassword <- BCrypt.hashpw[F](newUserInfo.password)
          user <- fromNewUserInfo(newUserInfo, hashedPassword).pure[F]
          maybeCreatedUser <- users.create(user)
          res <- maybeCreatedUser match
            case Left(error) => None.pure[F]
            case Right(id) => users.find(id)
        } yield res
    }

  override def changePassword(email: String, newPasswordInfo: NewPasswordInfo): F[Either[String, Option[User]]] =
    def checkAndUpdate(user: User, oldPassword: String, newPassword: String) =
      for {
        passCheck <- BCrypt.checkpwBool[F](
          newPasswordInfo.oldPassword,
          PasswordHash[BCrypt](user.hashedPassword)
        )
        updateRes <-
          if (passCheck)
            updateUserPassword(user, newPasswordInfo.newPassword)
          else Left("Invalid Password").pure[F]
      } yield updateRes

    def updateUserPassword(user: User, newPassword: String): F[Either[String, Option[User]]] =
      for {
        hashedPassword <- BCrypt.hashpw[F](newPassword)
        updatedUser <- users.update(user.id, user.copy(hashedPassword = hashedPassword))
      } yield Right(updatedUser)

    users.findByEmail(email).flatMap {
      case Some(user) => checkAndUpdate(user, newPasswordInfo.oldPassword, newPasswordInfo.newPassword)
      case None => Right(None).pure[F]
    }
}

object LiveAuth {
  def apply[F[_]: Async: Logger](users: Users[F], authenticator: Authenticator[F]): F[LiveAuth[F]] =
    new LiveAuth[F](users, authenticator).pure[F]
}