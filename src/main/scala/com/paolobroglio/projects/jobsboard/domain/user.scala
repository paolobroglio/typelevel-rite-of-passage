package com.paolobroglio.projects.jobsboard.domain

import com.paolobroglio.projects.jobsboard.domain.job.Job
import doobie.util.meta.Meta
import tsec.authorization.{AuthGroup, SimpleAuthEnum}

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
                       ) {
    def owns(job: Job): Boolean = email == job.ownerEmail
    def isAdmin: Boolean = role == Role.ADMIN
  }
  
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

  given roleAuthEnum: SimpleAuthEnum[Role, String] with {
    override val values: AuthGroup[Role] = AuthGroup(Role.ADMIN, Role.RECRUITER)
    override def getRepr(role: Role): String = role.toString
  }
}
