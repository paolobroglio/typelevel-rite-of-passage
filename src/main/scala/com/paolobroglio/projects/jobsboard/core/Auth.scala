package com.paolobroglio.projects.jobsboard.core

import cats.*
import cats.data.OptionT
import cats.effect.*
import cats.effect.kernel.MonadCancelThrow
import cats.implicits.*
import com.paolobroglio.projects.jobsboard.config.SecurityConfig
import com.paolobroglio.projects.jobsboard.domain.auth.NewPasswordInfo
import com.paolobroglio.projects.jobsboard.domain.error.{AppError, UserCreationError, UserNotFoundError, WrongPasswordError}
import com.paolobroglio.projects.jobsboard.domain.security.*
import com.paolobroglio.projects.jobsboard.domain.user.*
import org.http4s.Credentials

import concurrent.duration.DurationInt
import org.typelevel.log4cats.Logger
import tsec.authentication.{AugmentedJWT, IdentityStore, JWTAuthenticator}
import tsec.common.SecureRandomId
import tsec.authentication.BackingStore
import tsec.mac.jca.HMACSHA256
import tsec.passwordhashers.PasswordHash
import tsec.passwordhashers.jca.BCrypt

trait Auth[F[_]] {
  def login(email: String, password: String): F[Either[AppError, User]]
  def signUp(newUserInfo: NewUserInfo): F[Either[AppError, User]]
  def changePassword(email: String, newPasswordInfo: NewPasswordInfo): F[Either[AppError, User]]
  def deleteUser(id: Long): F[Option[AppError]]
  def logout(email: String): F[Either[AppError, User]]
}

class LiveAuth[F[_]: Async: Logger] private (users: Users[F]) extends Auth[F] {
  override def login(email: String, password: String): F[Either[AppError, User]] =
    users.findByEmail(email).flatMap {
      case Some(user) =>
        for {
          checkedPassword <- BCrypt.checkpwBool[F](
            password,
            PasswordHash[BCrypt](user.hashedPassword)
          )
          maybeUser <-
            if (checkedPassword)
              Right(user).pure[F]
            else
              Left(WrongPasswordError("Invalid password")).pure[F]
        } yield maybeUser
      case None =>
        Left(UserNotFoundError(s"User with email $email not found")).pure[F]
    }

  override def signUp(newUserInfo: NewUserInfo): F[Either[AppError, User]] =
    users.findByEmail(newUserInfo.email).flatMap {
      case Some(err) =>
        Left(UserCreationError("Email already registered")).pure[F]
      case None =>
        for {
          hashedPassword <- BCrypt.hashpw[F](newUserInfo.password)
          user <- fromNewUserInfo(newUserInfo, hashedPassword).pure[F]
          maybeCreatedUser <- users.create(user).flatMap {
            case Right(id) => users.find(id).flatMap {
              case Some(foundUser) => Right(foundUser).pure[F]
              case None => Left(UserNotFoundError(s"User not found with id $id")).pure[F]
            }
            case Left(error) => Left(UserCreationError(error)).pure[F]
          }
        } yield maybeCreatedUser
    }

  override def changePassword(email: String, newPasswordInfo: NewPasswordInfo): F[Either[AppError, User]] =

    def updateUserPassword(user: User, newPassword: String): F[Either[AppError, User]] =
      for {
        hashedPassword <- BCrypt.hashpw[F](newPassword)
        maybeUpdatedUser <- users.update(user.id, user.copy(hashedPassword = hashedPassword)).flatMap {
          case Some(user) =>
            Right(user).pure[F]
          case None => Left(UserNotFoundError(s"User with id ${user.id} not found")).pure[F]
        }
      } yield maybeUpdatedUser


    def checkAndUpdate(user: User, oldPassword: String, newPassword: String): F[Either[AppError, User]] =
      for {
        passCheck <- BCrypt.checkpwBool[F](
            newPasswordInfo.oldPassword,
            PasswordHash[BCrypt](user.hashedPassword)
          )
        updateRes <-
            if (passCheck)
              updateUserPassword(user, newPasswordInfo.newPassword)
            else Left(WrongPasswordError("Invalid password")).pure[F]
        } yield updateRes


    users.findByEmail(email).flatMap {
      case Some(user) => checkAndUpdate(user, newPasswordInfo.oldPassword, newPasswordInfo.newPassword)
      case None => Left(UserNotFoundError(s"User with email $email not found")).pure[F]
    }

  override def deleteUser(id: Long): F[Option[AppError]] =
    for {
      deleted <- users.delete(id)
      maybeError <-
        if (deleted)
          None.pure[F]
        else
          Some(UserNotFoundError(s"User with id $id wasn't found")).pure[F]
    } yield maybeError

  override def logout(email: String): F[Either[AppError, User]] = ???
}

object LiveAuth {
  def apply[F[_]: Async: Logger](users: Users[F]): F[LiveAuth[F]] = {
    new LiveAuth[F](users).pure[F]
  }
}