package com.paolobroglio.projects.jobsboard.core

import cats.effect.*
import cats.effect.implicits.*
import cats.effect.testing.scalatest.AsyncIOSpec
import com.paolobroglio.projects.jobsboard.fixtures.JobFixture
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers

class JobsSpec
  extends AsyncFreeSpec
    with AsyncIOSpec
    with Matchers
  with DoobieSpec
  with JobFixture {
  override val initScript: String = "sql/jobs.sql"

  "JobsSpec" - {
    "should return job" in {
      transactor.use {
        xa =>
          val program = for {
            jobs <- LiveJobs[IO](xa)
            retrieved <- jobs.find(AwesomeJobUuid)
          } yield retrieved

          program.asserting(_ shouldBe Some(AwesomeNewJob))
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
  }
}
