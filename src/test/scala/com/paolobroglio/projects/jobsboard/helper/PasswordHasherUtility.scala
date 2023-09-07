package com.paolobroglio.projects.jobsboard.helper

import cats.effect.{IO, IOApp}
import tsec.passwordhashers.PasswordHash
import tsec.passwordhashers.jca.BCrypt

object PasswordHasherUtility extends IOApp.Simple {
  override def run: IO[Unit] =
    BCrypt.hashpw[IO]("password1!").flatMap(IO.println) *>
      BCrypt
        .checkpwBool[IO](
          "password1!",
          PasswordHash[BCrypt]("$2a$10$7Adagujg935jeLzl6vQyPuwy6eckdMivlWaJzXkyo8vGG38ldbb12")
        )
        .flatMap(IO.println)
}
