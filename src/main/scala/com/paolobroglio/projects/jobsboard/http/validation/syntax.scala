package com.paolobroglio.projects.jobsboard.http.validation

import org.http4s.*
import org.http4s.implicits.*
import cats.*
import cats.data.*
import cats.data.Validated.*
import cats.implicits.*
import com.paolobroglio.projects.jobsboard.http.responses.FailureResponse
import com.paolobroglio.projects.jobsboard.http.validation.validators.{ValidationResult, Validator}
import org.http4s.circe.CirceEntityCodec.*
import io.circe.generic.auto.*
import org.typelevel.log4cats.Logger
import com.paolobroglio.projects.jobsboard.logging.syntax.*
import org.http4s.dsl.Http4sDsl
import org.http4s.dsl.io.BadRequest

object syntax {

  def validateEntity[A](entity: A)(using validator: Validator[A]): ValidationResult[A] =
    validator.validate(entity)

  trait HttpValidationDsl[F[_]: MonadThrow: Logger] extends Http4sDsl[F] {
    extension (req: Request[F])
      def validate[A: Validator](serverLogicIfValid: A => F[Response[F]])(using EntityDecoder[F,A]): F[Response[F]] =
        req
          .as[A]
          .logError(e => s"Parsing payload failed: $e")
          .map(validateEntity)
          .flatMap {
            case Valid(entity) =>
              serverLogicIfValid(entity)
            case Invalid(errors) =>
              BadRequest(FailureResponse(errors.toList.map(_.errorMessage).mkString(", ")))
          }
  }

}
