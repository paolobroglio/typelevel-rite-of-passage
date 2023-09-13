package com.paolobroglio.projects.jobsboard.http.validation

import com.paolobroglio.projects.jobsboard.domain.job.JobInfo
import cats.*
import cats.implicits.*
import cats.data.*
import cats.data.Validated.*
import com.paolobroglio.projects.jobsboard.domain.auth.{LoginInfo, NewPasswordInfo}
import com.paolobroglio.projects.jobsboard.domain.user.NewUserInfo

import java.net.URL
import scala.util.{Failure, Success, Try}
object validators {

  sealed trait ValidationFailure(val errorMessage: String)
  case class EmptyField(fieldName: String) extends ValidationFailure(s"'$fieldName is empty")
  case class InvalidUrl(fieldName: String) extends ValidationFailure(s"'$fieldName is not a valid URL")

  type ValidationResult[A] = ValidatedNel[ValidationFailure, A]

  trait Validator[A] {
    def validate(value: A): ValidationResult[A]
  }

  def validateRequired[A](field: A, fieldName: String)(required: A => Boolean): ValidationResult[A] =
  if (required(field)) field.validNel
  else EmptyField(fieldName).invalidNel

  def validateUrl(field: String, fieldName: String): ValidationResult[String] =
    Try(URL(field).toURI) match {
      case Failure(exception) => InvalidUrl(fieldName).invalidNel
      case Success(value) => field.validNel
    }

  given jobInfoValidator: Validator[JobInfo] = (jobInfo: JobInfo) => {
    val JobInfo(
      company,
      title,
      description,
      externalUrl,
      remote,
      location,
      salaryLo,
      salaryHi,
      currency,
      country,
      tags,
      image,
      seniority,
      other
    ) = jobInfo

    val validCompany = validateRequired(company, "company")(_.nonEmpty)
    val validTitle= validateRequired(title, "title")(_.nonEmpty)
    val validDescription = validateRequired(description, "description")(_.nonEmpty)
    val validExternalUrl = validateUrl(externalUrl, "externalUrl")
    val validLocation = validateRequired(location, "location")(_.nonEmpty)

    (
      validCompany,
      validTitle,
      validDescription,
      validExternalUrl,
      remote.validNel,
      validLocation,
      salaryLo.validNel,
      salaryHi.validNel,
      currency.validNel,
      country.validNel,
      tags.validNel,
      image.validNel,
      seniority.validNel,
      other.validNel
    ).mapN(JobInfo.apply)
  }

  given loginInfoValidator: Validator[LoginInfo] = (loginInfo: LoginInfo) => {
    val LoginInfo(
      email,
      password
    ) = loginInfo

    val validEmail = validateRequired(email, "email")(_.nonEmpty)
    val validPassword = validateRequired(password, "password")(_.nonEmpty)

    (
      validEmail,
      validPassword
    ).mapN(LoginInfo.apply)
  }

  given newUserInfoValidator: Validator[NewUserInfo] = (newUserInfo: NewUserInfo) => {
    val NewUserInfo(
      email,
      password,
      firstName,
      lastName,
      company
    ) = newUserInfo

    val validEmail = validateRequired(email, "email")(_.nonEmpty)
    val validPassword = validateRequired(password, "password")(_.nonEmpty)

    (
      validEmail,
      validPassword,
      firstName.validNel,
      lastName.validNel,
      company.validNel
    ).mapN(NewUserInfo.apply)
  }

}
