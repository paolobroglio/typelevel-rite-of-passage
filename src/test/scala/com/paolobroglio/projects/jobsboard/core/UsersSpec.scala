package com.paolobroglio.projects.jobsboard.core

import cats.effect.*
import cats.effect.implicits.*
import cats.effect.testing.scalatest.AsyncIOSpec
import com.paolobroglio.projects.jobsboard.fixtures.UserFixture
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

class UsersSpec
  extends AsyncFreeSpec
    with AsyncIOSpec
    with Matchers
    with DoobieSpec
    with UserFixture {

  override val initScript: String = "sql/users.sql"

  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  "UsersSpec" - {
    "should retrieve a user by id" in {
      transactor.use { xa =>
        val program = for {
          users <- LiveUsers[IO](xa)
          retrieved <- users.find(MarioRossi.id)
        } yield retrieved

        program.asserting(_ shouldBe Some(MarioRossi))
      }
    }
    "should retrieve a user by email" in {
      transactor.use { xa =>
        val program = for {
          users <- LiveUsers[IO](xa)
          retrieved <- users.findByEmail(MarioRossi.email)
        } yield retrieved

        program.asserting(_ shouldBe Some(MarioRossi))
      }
    }
    "should create a new user" in {
      transactor.use { xa =>
        val program = for {
          users <- LiveUsers[IO](xa)
          maybeUser <- users.create(NewUser)
          retrieved <- maybeUser match
            case Left(error) => fail()
            case Right(userId) => users.find(userId)
        } yield retrieved

        program.asserting(_ shouldBe a [Some[_]])
      }
    }
    "should fail creating a new user with existing email" in {
      transactor.use { xa =>
        val program = for {
          users <- LiveUsers[IO](xa)
          result <- users.create(NewUser.copy(email = MarioRossi.email))
        } yield result

        program.asserting(_ shouldBe Left("E-Mail Already exists"))
      }
    }
    "should update a user" in {
      transactor.use { xa =>
        val program = for {
          users <- LiveUsers[IO](xa)
          updated <- users.update(UpdatedMario.id, UpdatedMario)
        } yield updated

        program.asserting {
          case Some(updated) =>
            updated.email shouldBe UpdatedMario.email
          case None =>
            fail()
        }
      }
    }
  }
}
