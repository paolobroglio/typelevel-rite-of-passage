package com.paolobroglio.projects.jobsboard.core

import cats.effect.testing.scalatest.AsyncIOSpec
import com.paolobroglio.projects.jobsboard.fixtures.UserFixture
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger
import cats.effect.*
import cats.effect.implicits.*
import com.paolobroglio.projects.jobsboard.config.TokenConfig
import com.paolobroglio.projects.jobsboard.domain.user.User
import org.typelevel.log4cats.slf4j.Slf4jLogger
import concurrent.duration.DurationInt

class TokensSpec extends AsyncFreeSpec
  with AsyncIOSpec
  with Matchers
  with DoobieSpec
  with UserFixture {

  override val initScript: String = "sql/recoverytokens.sql"
  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  private val tokenConfig = TokenConfig(1000L)

  private val mockedUsers: Users[IO] = new Users[IO]:
    override def find(id: Long): IO[Option[User]] =
      if (id == MarioRossi.id) IO.pure(Some(MarioRossi))
      else if (id == 0L) IO.pure(Some(AverageJoe))
      else IO.pure(None)

    override def create(user: User): IO[Either[String, Long]] = IO.pure(Right(user.id))

    override def update(id: Long, user: User): IO[Option[User]] = IO.pure(Some(user))

    override def delete(id: Long): IO[Boolean] = IO.pure(true)

    override def findByEmail(email: String): IO[Option[User]] =
      if (email == MarioRossi.email) IO.pure(Some(MarioRossi))
      else if (email == JohnDoe.email) IO.pure(Some(JohnDoe))
      else IO.pure(None)


  "Tokens" - {
    "should retrieve a token" in {
      transactor.use { xa =>
        val program = for {
          tokens <- LiveTokens[IO](mockedUsers)(xa, TokenConfig(15000L))
          retrieved <- tokens.getToken(MarioRossi.email)
        } yield retrieved

        program.asserting {
          case Some(value) => succeed
          case None => fail()
        }
      }
    }
    "should not retrieve a token for absent user" in {
      transactor.use { xa =>
        val program = for {
          tokens <- LiveTokens[IO](mockedUsers)(xa, TokenConfig(15000L))
          retrieved <- tokens.getToken(AverageJoe.email)
        } yield retrieved

        program.asserting(_ shouldBe None)
      }
    }
    "should generate a token for user without token" in {
      transactor.use { xa =>
        val program = for {
          tokens <- LiveTokens[IO](mockedUsers)(xa, TokenConfig(15000L))
          retrieved <- tokens.getToken(JohnDoe.email)
        } yield retrieved

        program.asserting {
          case Some(value) => succeed
          case None => fail()
        }
      }
    }
    "should check a good token and accept it" in {
      transactor.use { xa =>
        val program = for {
          tokens <- LiveTokens[IO](mockedUsers)(xa, TokenConfig(15000))
          maybeToken <- tokens.getToken(MarioRossi.email)
          _ <- IO.sleep(500.millis)
          isValid <- maybeToken match
            case Some(token) => tokens.checkToken(MarioRossi.email, token)
            case None => IO.pure(false)
        } yield isValid

        program.asserting(_ shouldBe true)
      }
    }
    "should check an expired token and reject it" in {
      transactor.use { xa =>
        val program = for {
          tokens <- LiveTokens[IO](mockedUsers)(xa, TokenConfig(100))
          maybeToken <- tokens.getToken(MarioRossi.email)
          _ <- IO.sleep(500.millis)
          isValid <- maybeToken match
            case Some(token) => tokens.checkToken(MarioRossi.email, token)
            case None => IO.pure(false)
        } yield isValid

        program.asserting(_ shouldBe false)
      }
    }
  }

}
