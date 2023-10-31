package com.paolobroglio.projects.jobsboard.http.routes

import cats.effect.*
import cats.implicits.*
import cats.{Monad, MonadThrow}
import com.paolobroglio.projects.jobsboard.core.{Auth, Jobs}
import com.paolobroglio.projects.jobsboard.domain.job.{Job, JobFilter, JobInfo}
import com.paolobroglio.projects.jobsboard.domain.pagination.Pagination
import com.paolobroglio.projects.jobsboard.http.responses.FailureResponse
import com.paolobroglio.projects.jobsboard.domain.security.*
import com.paolobroglio.projects.jobsboard.domain.user.User
import io.circe.generic.auto.*
import org.http4s.HttpRoutes
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.dsl.Http4sDsl
import org.http4s.server.Router
import org.typelevel.log4cats.Logger
import com.paolobroglio.projects.jobsboard.logging.syntax.*
import com.paolobroglio.projects.jobsboard.http.validation.syntax.*
import tsec.authentication.{SecuredRequestHandler, asAuthed}

import java.util.UUID
import scala.collection.mutable

class JobRoutes[F[_]: Concurrent: Logger] private (jobs: Jobs[F], authenticator: Authenticator[F]) extends HttpValidationDsl[F] {

  private val securedHandler: SecuredRequestHandler[F, String, User, JwtToken] = SecuredRequestHandler(authenticator)

  object OffsetQueryParam extends OptionalQueryParamDecoderMatcher[Int]("offset")
  object LimitQueryParam extends OptionalQueryParamDecoderMatcher[Int]("limit")

  private val allJobsRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ POST -> Root / "all" :? OffsetQueryParam(offset) +& LimitQueryParam(limit) =>
      for {
        filter <- req.as[JobFilter].logError(e => s"Error parsing filter ${e.getMessage}")
        _ <- Logger[F].info("Find All Jobs called")
        allJobs <- jobs.all(filter, Pagination(limit, offset)).logError(e => s"Failed getting jobs: $e")
        resp <- Ok(allJobs)
      } yield resp
  }

  private val findJobRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root / UUIDVar(id) =>
      jobs.find(id).logError(e => s"Failed getting job: $e").flatMap {
        case Some(job) =>
          for {
            _ <- Logger[F].info(s"Found Job $job")
            resp <- Ok(job)
          } yield resp
        case None => NotFound(FailureResponse(s"Job $id not found"))
      }
  }

  private val createJobRoute: AuthRoute[F] = {
    case req @ POST -> Root asAuthed _ =>
      req.request.validate[JobInfo] { jobInfo =>
        for {
          _ <- Logger[F].info("Create Job called")
          id <- jobs.create("TODO@something.com", jobInfo).logError(e => s"Failed creating job: $e")
          _ <- Logger[F].info(s"Created Job $id")
          resp <- Created(id)
        } yield resp
      }
  }

  private val updateJobRoute: AuthRoute[F] = {
    case req @ PUT -> Root / UUIDVar(id) asAuthed user =>
      req.request.validate[JobInfo] {
        jobInfo =>
          jobs.find(id).flatMap {
            case None =>
              NotFound(FailureResponse(s"Job $id not found"))
            case Some(job) if user.owns(job) || user.isAdmin =>
              for {
                _ <- Logger[F].info("Update Job called")
                _ <- jobs.update(id, jobInfo)
                _ <- Logger[F].info(s"Updated JobInfo $jobInfo")
                  resp <- Ok()
              } yield resp
            case _ => Forbidden(FailureResponse(s"You don't own Job $id"))
          }
      }
  }

  private val deleteJobRoute: AuthRoute[F] = {
    case req @ DELETE -> Root / UUIDVar(id) asAuthed user =>
      jobs.find(id).flatMap {
        case None =>
          NotFound(FailureResponse(s"Job $id not found"))
        case Some(job) if user.owns(job) || user.isAdmin =>
          for {
            _ <- Logger[F].info("Delete Job called")
              affected <- jobs.delete(id)
            _ <- Logger[F].info(s"Deleted Jobs $affected")
              resp <- Ok()
          } yield resp
        case _ => Forbidden(FailureResponse(s"You don't own Job $id"))
      }
  }

  val nonAuthedRoutes = allJobsRoute <+> findJobRoute
  val authedRoutes = securedHandler.liftService(
    createJobRoute.restrictedTo(allRoles) |+|
      deleteJobRoute.restrictedTo(allRoles) |+|
      updateJobRoute.restrictedTo(allRoles)
  )

  val routes: HttpRoutes[F] = Router(
    "/jobs" -> (nonAuthedRoutes <+> authedRoutes)
  )
}

object JobRoutes {
  def apply[F[_]: Concurrent: Logger](jobs: Jobs[F], authenticator: Authenticator[F]) = new JobRoutes[F](jobs, authenticator)
}