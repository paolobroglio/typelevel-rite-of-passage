package com.paolobroglio.projects.jobsboard.http.routes

import cats.effect.*
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.implicits.*
import com.paolobroglio.projects.jobsboard.core.Jobs
import com.paolobroglio.projects.jobsboard.domain.job
import com.paolobroglio.projects.jobsboard.domain.job.Job
import com.paolobroglio.projects.jobsboard.fixtures.JobFixture
import io.circe.generic.auto.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.dsl.Http4sDsl
import org.http4s.implicits.uri
import org.http4s.{HttpRoutes, Method, Request, Status}
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.shouldBe
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.util.UUID

class JobRoutesSpec
  extends AsyncFreeSpec
    with AsyncIOSpec
    with Matchers
    with Http4sDsl[IO]
    with JobFixture {

  val jobs: Jobs[IO] = new Jobs[IO]:
    override def create(ownerEmail: String, jobInfo: job.JobInfo): IO[UUID] = IO.pure(NewJobUuid)

    override def all(): IO[List[job.Job]] = IO.pure(List(AwesomeJob))

    override def find(id: UUID): IO[Option[job.Job]] =
      if (id == AwesomeJob.id)
        IO.pure(Some(AwesomeJob))
      else
        IO.pure(None)

    override def update(id: UUID, jobInfo: job.JobInfo): IO[Option[job.Job]] =
      if (id == AwesomeJob.id)
        IO.pure(Some(UpdatedAwesomeJob))
      else
        IO.pure(None)

    override def delete(id: UUID): IO[Int] =
      if (id == AwesomeJob.id)
        IO.pure(1)
      else
        IO.pure(0)

  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  val jobRoutes: HttpRoutes[IO] = JobRoutes[IO](jobs).routes


  "JobRoutes" - {
    "should return a job with a given id" in {
      for {
        response <- jobRoutes.orNotFound.run(
          Request(method = Method.GET, uri = uri"/jobs/843df718-ec6e-4d49-9289-f799c0f40064")
        )
        retrieved <- response.as[Job]
      } yield {
        response.status shouldBe Status.Ok
        retrieved shouldBe AwesomeJob
      }
    }
    "should return all jobs" in {
      for {
        response <- jobRoutes.orNotFound.run(
          Request(method = Method.GET, uri = uri"/jobs/")
        )
        retrieved <- response.as[List[Job]]
      } yield {
        response.status shouldBe Status.Ok
        retrieved shouldBe List(AwesomeJob)
      }
    }
    "should update a job with a given id" in {
      for {
        response <- jobRoutes.orNotFound.run(
          Request(
            method = Method.PUT,
            uri = uri"/jobs/843df718-ec6e-4d49-9289-f799c0f40064"
          ).withEntity(UpdateJobInfoAwesomeJob)
        )
      } yield {
        response.status shouldBe Status.Ok
      }
    }
    "should create a new job" in {
      for {
        response <- jobRoutes.orNotFound.run(
          Request(
            method = Method.POST,
            uri = uri"/jobs/"
          ).withEntity(AwesomeNewJob)
        )
        newJobId <- response.as[UUID]
      } yield {
        response.status shouldBe Status.Created
        newJobId.toString shouldBe NewJobUuid.toString
      }
    }
  }
}