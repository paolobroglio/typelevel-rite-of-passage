package com.paolobroglio.projects.jobsboard.http.routes

import cats.data.OptionT
import cats.effect.*
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.implicits.*
import com.paolobroglio.projects.jobsboard.core.Jobs
import com.paolobroglio.projects.jobsboard.domain.job
import com.paolobroglio.projects.jobsboard.domain.job.{Job, JobFilter}
import com.paolobroglio.projects.jobsboard.domain.pagination.Pagination
import com.paolobroglio.projects.jobsboard.domain.security.{Authenticator, JwtToken}
import com.paolobroglio.projects.jobsboard.domain.user.User
import com.paolobroglio.projects.jobsboard.fixtures.{JobFixture, UserFixture}
import io.circe.generic.auto.*

import concurrent.duration.DurationInt
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.Authorization
import org.http4s.implicits.uri
import org.http4s.{AuthScheme, Credentials, HttpRoutes, Method, Request, Status, Uri}
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.shouldBe
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import tsec.authentication.{IdentityStore, JWTAuthenticator}
import tsec.jws.mac.JWTMac
import tsec.mac.jca.HMACSHA256

import java.util.UUID

class JobRoutesSpec
  extends AsyncFreeSpec
    with AsyncIOSpec
    with Matchers
    with Http4sDsl[IO]
    with JobFixture
    with UserFixture {

  val jobs: Jobs[IO] = new Jobs[IO]:
    override def create(ownerEmail: String, jobInfo: job.JobInfo): IO[UUID] = IO.pure(NewJobUuid)

    override def all(filter: JobFilter, pagination: Pagination): IO[List[job.Job]] =
      if (filter.remote) IO.pure(List())
      else IO.pure(List(AwesomeJob))

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

  val mockedAuthenticator: Authenticator[IO] = {
    val key = HMACSHA256.unsafeGenerateKey
    val idStore: IdentityStore[IO, String, User] = (email: String) =>
      if (email == AverageJoe.email) OptionT.pure(AverageJoe)
      else if (email == JohnDoe.email) OptionT.pure(JohnDoe)
      else OptionT.none[IO, User]

    JWTAuthenticator.unbacked.inBearerToken(
      1.day,
      None,
      idStore,
      key
    )
  }

  val jobRoutes: HttpRoutes[IO] = JobRoutes[IO](jobs, mockedAuthenticator).routes

  extension (r: Request[IO])
    def withBearerToken(a: JwtToken): Request[IO] =
      r.putHeaders {
        val jwtString = JWTMac.toEncodedString[IO, HMACSHA256](a.jwt)

        Authorization(Credentials.Token(AuthScheme.Bearer, jwtString))
      }

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
    "should return 404 for an absent job" in {
      for {
        response <- jobRoutes.orNotFound.run(
          Request(method = Method.GET, uri = uri"/jobs/843df718-ec6e-4d49-9289-f799c0f400ff")
        )
      } yield {
        response.status shouldBe Status.NotFound
      }
    }
    "should return all jobs" in {
      for {
        response <- jobRoutes.orNotFound.run(
          Request(method = Method.POST, uri = uri"/jobs/all")
            .withEntity(JobFilter())
        )
        retrieved <- response.as[List[Job]]
      } yield {
        response.status shouldBe Status.Ok
        retrieved shouldBe List(AwesomeJob)
      }
    }
    "should update a job with a given id" in {
      for {
        jwt <- mockedAuthenticator.create(AverageJoe.email)
        response <- jobRoutes.orNotFound.run(
          Request(
            method = Method.PUT,
            uri = uri"/jobs/843df718-ec6e-4d49-9289-f799c0f40064"
          ).withBearerToken(jwt)
            .withEntity(UpdateJobInfoAwesomeJob)
        )
      } yield {
        response.status shouldBe Status.Ok
      }
    }
    "should return 404 for an absent job on update" in {
      for {
        jwt <- mockedAuthenticator.create(AverageJoe.email)
        response <- jobRoutes.orNotFound.run(
          Request(method = Method.PUT, uri = uri"/jobs/843df718-ec6e-4d49-9289-f799c0f400ff")
            .withBearerToken(jwt)
            .withEntity(UpdateJobInfoAwesomeJob)
        )
      } yield {
        response.status shouldBe Status.NotFound
      }
    }
    "should create a new job" in {
      for {
        jwt <- mockedAuthenticator.create(AverageJoe.email)
        response <- jobRoutes.orNotFound.run(
          Request(
            method = Method.POST,
            uri = uri"/jobs/"
          ).withBearerToken(jwt)
            .withEntity(AwesomeNewJob)
        )
        newJobId <- response.as[UUID]
      } yield {
        response.status shouldBe Status.Created
        newJobId.toString shouldBe NewJobUuid.toString
      }
    }
    "should delete a job" in {
      for {
        jwt <- mockedAuthenticator.create(AverageJoe.email)
        response <- jobRoutes.orNotFound.run(
          Request(
            method = Method.DELETE,
            uri = uri"/jobs/843df718-ec6e-4d49-9289-f799c0f40064"
          ).withBearerToken(jwt)
        )
      } yield {
        response.status shouldBe Status.Ok
      }
    }
    "should return 404 when deleting an absent job" in {
      for {
        jwt <- mockedAuthenticator.create(AverageJoe.email)
        response <- jobRoutes.orNotFound.run(
          Request(
            method = Method.DELETE,
            uri = uri"/jobs/843df718-ec6e-4d49-9289-f799c0f400ff"
          ).withBearerToken(jwt)
        )
      } yield {
        response.status shouldBe Status.NotFound
      }
    }
  }
}
