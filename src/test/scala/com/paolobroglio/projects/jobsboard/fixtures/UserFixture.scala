package com.paolobroglio.projects.jobsboard.fixtures

import cats.effect.IO
import com.paolobroglio.projects.jobsboard.core.Users
import com.paolobroglio.projects.jobsboard.domain.user.*

import java.util.UUID

trait UserFixture {
  val mockedUsers: Users[IO] = new Users[IO] {
    override def find(id: Long): IO[Option[User]] =
      if (id == JohnDoe.id) IO.pure(Some(JohnDoe))
      else if (id == AverageJoe.id) IO.pure(Some(AverageJoe))
      else IO.pure(None)

    override def create(user: User): IO[Either[String, Long]] = IO.pure(Right(user.id))

    override def update(id:Long, user: User): IO[Option[User]] = IO.pure(Some(user))

    override def delete(id: Long): IO[Boolean] = IO.pure(true)

    override def findByEmail(email: String): IO[Option[User]] =
      if (email == JohnDoe.email) IO.pure(Some(JohnDoe))
      else IO.pure(None)
  }

  val AverageJoeSignUp = NewUserInfo(
    email = "joe@gmail.com",
    password = "$2a$10$7Adagujg935jeLzl6vQyPuwy6eckdMivlWaJzXkyo8vGG38ldbb12",
    firstName = Some("Average"),
    lastName = Some("Joe"),
    company = None
  )

  val AverageJoe = User(
    id = 15L,
    email = "joe@gmail.com",
    hashedPassword = "$2a$10$7Adagujg935jeLzl6vQyPuwy6eckdMivlWaJzXkyo8vGG38ldbb12",
    firstName = Some("Average"),
    lastName = Some("Joe"),
    company = None,
    role = Role.RECRUITER
  )

  val JohnDoeSignUp = NewUserInfo(
    email = "john@company.com",
    password = "$2a$10$7Adagujg935jeLzl6vQyPuwy6eckdMivlWaJzXkyo8vGG38ldbb12",
    firstName = Some("John"),
    lastName = Some("Doe"),
    company = None
  )

  val JohnDoe = User(
    10L,
    "john@company.com",
    "$2a$10$7Adagujg935jeLzl6vQyPuwy6eckdMivlWaJzXkyo8vGG38ldbb12",
    Some("John"),
    Some("Doe"),
    Some("Company SPA"),
    Role.ADMIN
  )
  val johnEmail = JohnDoe.email
  val johnPassword = "rockthejvm"

  val MarioRossi = User(
    11L,
    "mario@company.com",
    "$2a$10$7Adagujg935jeLzl6vQyPuwy6eckdMivlWaJzXkyo8vGG38ldbb12",
    Some("Mario"),
    Some("Rossi"),
    Some("Company SPA"),
    Role.RECRUITER
  )
  val marioEmail = MarioRossi.email
  val marioPassword = "riccardorulez"

  val NewUser = User(
    0L,
    "newuser@gmail.com",
    "$2a$10$7Adagujg935jeLzl6vQyPuwy6eckdMivlWaJzXkyo8vGG38ldbb12",
    Some("Albert"),
    Some("Oppy"),
    Some("Caltech"),
    Role.RECRUITER
  )

  val UpdatedMario = User(
    11L,
    "mario@gmail.com",
    "$2a$10$7Adagujg935jeLzl6vQyPuwy6eckdMivlWaJzXkyo8vGG38ldbb12",
    Some("MARIO"),
    Some("ROSSI"),
    Some("Inforooms"),
    Role.RECRUITER
  )
}
