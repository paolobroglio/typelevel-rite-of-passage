package com.paolobroglio.projects.jobsboard.core

import cats.effect.*
import cats.effect.implicits.*
import cats.effect.testing.scalatest.AsyncIOSpec
import com.paolobroglio.projects.jobsboard.fixtures.JobFixture
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

class JobsSpec
  extends AsyncFreeSpec
    with AsyncIOSpec
    with Matchers
  with DoobieSpec
  with JobFixture {

  override val initScript: String = "sql/jobs.sql"
  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  "JobsSpec" - {
    "should return job" in {
      transactor.use {
        xa =>
          val program = for {
            jobs <- LiveJobs[IO](xa)
            retrieved <- jobs.find(AwesomeJobUuid)
          } yield retrieved

          program.asserting(_ shouldBe Some(AwesomeJob))
      }
    }
    "should return no job if the given UUID does not exist" in {
      transactor.use {
        xa =>
          val program = for {
            jobs <- LiveJobs[IO](xa)
            retrieved <- jobs.find(NotFoundJobUuid)
          } yield retrieved

          program.asserting(_ shouldBe None)
      }
    }
    "should update job" in {
      transactor.use {
        xa =>
          val program = for {
            jobs <- LiveJobs[IO](xa)
            updated <- jobs.update(AwesomeJobUuid, UpdateJobInfoAwesomeJob)
          } yield updated

          program.asserting(_ shouldBe Some(UpdatedAwesomeJob))
      }
    }
    "should create job" in {
      transactor.use {
        xa =>
          val program = for {
            jobs <- LiveJobs[IO](xa)
            createdId <- jobs.create("daniel@rockthejvm.com", AwesomeNewJob)
            retrieved <- jobs.find(createdId).map(_.map(_.jobInfo))
          } yield retrieved

          program.asserting(_ shouldBe Some(AwesomeNewJob))
      }
    }
    "should delete job" in {
      transactor.use {
        xa =>
          val program = for {
            jobs <- LiveJobs[IO](xa)
            createdId <- jobs.create("daniel@rockthejvm.com", AwesomeNewJob)
            retrieved <- jobs.find(createdId).map(_.map(_.jobInfo))
          } yield retrieved

          program.asserting(_ shouldBe Some(AwesomeNewJob))
      }
    }
  }
}
