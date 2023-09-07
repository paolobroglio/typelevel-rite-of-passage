package com.paolobroglio.projects.jobsboard.core

import cats.*
import cats.effect.*
import cats.effect.kernel.MonadCancelThrow
import cats.implicits.*
import com.paolobroglio.projects.jobsboard.domain.job.{Job, JobFilter, JobInfo}
import com.paolobroglio.projects.jobsboard.domain.pagination.Pagination
import com.paolobroglio.projects.jobsboard.domain.user.*
import com.paolobroglio.projects.jobsboard.logging.syntax.*
import doobie.*
import doobie.implicits.*
import doobie.postgres.implicits.*
import doobie.postgres.sqlstate
import doobie.util.*
import org.typelevel.log4cats.Logger

import java.util.UUID

trait Users[F[_]] {
  def find(id: Long): F[Option[User]]
  def findByEmail(email: String): F[Option[User]]
  def create(user: User): F[Either[String, Long]]
  def update(id: Long, user: User): F[Option[User]]
  def delete(id: Long): F[Boolean]
}

final class LiveUsers[F[_]: MonadCancelThrow: Logger] private (xa: Transactor[F]) extends Users[F] {
  override def find(id: Long): F[Option[User]] =
    sql"""
         SELECT *
         FROM users
         WHERE id = $id
       """
      .query[User]
      .option
      .transact(xa)

  override def findByEmail(email: String): F[Option[User]] =
    sql"""
         SELECT *
         FROM users
         WHERE email = $email
       """
      .query[User]
      .option
      .transact(xa)

  override def create(user: User): F[Either[String, Long]] =
    sql"""
         INSERT INTO users (
            email,
            hashedPassword,
            firstName,
            lastName,
            company,
            role
        ) VALUES (
          ${user.email},
          ${user.hashedPassword},
          ${user.firstName},
          ${user.lastName},
          ${user.company},
          ${user.role}
        )
       """
      .update
      .withUniqueGeneratedKeys[Long]("id")
      .transact(xa)
      .attemptSomeSqlState {
        case sqlstate.class23.UNIQUE_VIOLATION => "E-Mail Already exists"
      }

  override def update(id: Long, user: User): F[Option[User]] =
    sql"""
         UPDATE users
         SET
            email=${user.email},
            hashedPassword=${user.hashedPassword},
            firstName=${user.firstName},
            lastName=${user.lastName},
            company=${user.company},
            role=${user.role}
         WHERE id=$id
       """
      .update
      .run
      .transact(xa)
      .attemptSomeSqlState {
        case sqlstate.class23.UNIQUE_VIOLATION => "E-Mail Already exists"
      }
      .flatMap(_ => find(id))

  override def delete(id: Long): F[Boolean] =
    sql"""
         DELETE FROM users
         WHERE id = $id
       """
      .update
      .run
      .transact(xa)
      .map(_ > 0)
}

object LiveUsers {
  given userRead: Read[User] = Read[(
      Long,
      String,
      String,
      Option[String],
      Option[String],
      Option[String],
      Role
    )].map {
    case (
      id: Long,
      email: String,
      hashedPassword: String,
      firstName: Option[String],
      lastName: Option[String],
      company: Option[String],
      role: Role
      ) => User(
      id = id,
      email = email,
      hashedPassword = hashedPassword,
      firstName = firstName,
      lastName = lastName,
      company = company,
      role = role
    )
  }

  def apply[F[_]: MonadCancelThrow: Logger](xa: Transactor[F]): F[LiveUsers[F]] =
    new LiveUsers[F](xa).pure[F]
}