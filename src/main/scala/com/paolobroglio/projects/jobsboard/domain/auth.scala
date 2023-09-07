package com.paolobroglio.projects.jobsboard.domain

object auth {
  final case class LoginInfo(
      email: String,
      password: String
                            )

  final case class NewPasswordInfo(
      oldPassword: String,
      newPassword: String
                                  )
}
