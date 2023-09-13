package com.paolobroglio.projects.jobsboard.domain

object error {
  sealed trait AppError(message: String)

  case class WrongPasswordError(message: String) extends AppError(message)
  case class UserNotFoundError(message: String) extends AppError(message)
  case class UserCreationError(message: String) extends AppError(message)
}
