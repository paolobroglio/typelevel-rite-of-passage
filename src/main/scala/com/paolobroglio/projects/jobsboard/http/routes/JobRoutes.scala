package com.paolobroglio.projects.jobsboard.http.routes

import cats.effect.*
import cats.implicits.*
import cats.{Monad, MonadThrow}
import com.paolobroglio.projects.jobsboard.core.Jobs
import com.paolobroglio.projects.jobsboard.domain.job.{Job, JobInfo}
import com.paolobroglio.projects.jobsboard.http.responses.FailureResponse
import io.circe.generic.auto.*
import org.http4s.HttpRoutes
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.dsl.Http4sDsl
import org.http4s.server.Router
import org.typelevel.log4cats.Logger
import com.paolobroglio.projects.jobsboard.logging.syntax.*

import java.util.UUID
import scala.collection.mutable

class JobRoutes[F[_]: Concurrent: Logger] private (jobs: Jobs[F]) extends Http4sDsl[F] {

  private val allJobsRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root =>
      for {
        _ <- Logger[F].info("Find All Jobs called")
        allJobs <- jobs.all().logError(e => s"Failed getting jobs: $e")
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

  private val createJobRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ POST -> Root =>
      for {
        _ <- Logger[F].info("Create Job called")
        jobInfo <- req.as[JobInfo].logError(e => s"Failed parsing JobInfo: $e")
        id <- jobs.create("TODO@something.com", jobInfo).logError(e => s"Failed creating job: $e")
        _ <- Logger[F].info(s"Created Job $id")
        resp <- Created(id)
      } yield resp
  }

  private val updateJobRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ PUT -> Root / UUIDVar(id) =>
      jobs.find(id).flatMap {
        case Some(job) =>
          for {
            _ <- Logger[F].info("Update Job called")
            jobInfo <- req.as[JobInfo].logError(e => s"Failed parsing JobInfo: $e")
            _ <- jobs.update(id, jobInfo)
            _ <- Logger[F].info(s"Updated JobInfo $jobInfo")
            resp <- Ok()
          } yield resp
        case None =>
          NotFound(FailureResponse(s"Job $id not found"))
      }
  }

  private val deleteJobRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ DELETE -> Root / UUIDVar(id) =>
      jobs.find(id).flatMap {
        case Some(job) =>
          for {
            _ <- Logger[F].info("Delete Job called")
            affected <- jobs.delete(id)
            _ <- Logger[F].info(s"Deleted Jobs $affected")
            resp <- Ok()
          } yield resp
        case None =>
          NotFound(FailureResponse(s"Job $id not found"))
      }
  }

  val routes: HttpRoutes[F] = Router(
    "/jobs" -> (allJobsRoute <+> findJobRoute <+> createJobRoute <+> updateJobRoute <+> deleteJobRoute)
  )
}

object JobRoutes {
  def apply[F[_]: Concurrent: Logger](jobs: Jobs[F]) = new JobRoutes[F](jobs)
}