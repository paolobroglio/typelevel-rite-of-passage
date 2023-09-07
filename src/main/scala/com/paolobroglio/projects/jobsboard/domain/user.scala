package com.paolobroglio.projects.jobsboard.domain

import doobie.util.meta.Meta

import java.util.UUID

object user {
  final case class User(
      id: Long = 0L,
      email: String,
      hashedPassword: String,
      firstName: Option[String],
      lastName: Option[String],
      company: Option[String],
      role: Role
                       )
  
  final case class NewUserInfo(
                                email: String, 
                                password: String, 
                                firstName: Option[String], 
                                lastName: Option[String], 
                                company: Option[String]
                              )

  def fromNewUserInfo(newUserInfo: NewUserInfo, password: String): User =
    User(
      email = newUserInfo.email,
      hashedPassword = password,
      firstName = newUserInfo.firstName,
      lastName = newUserInfo.lastName,
      company = newUserInfo.company,
      role = Role.RECRUITER
    )

  enum Role {
    case ADMIN, // access to all endpoints
         RECRUITER // able to post jobs
  }

  object Role {
    given metaRole: Meta[Role] =
      Meta[String].timap[Role](Role.valueOf)(_.toString)
  }
}
