CREATE DATABASE board;
\c board;

CREATE TABLE jobs(
 id uuid DEFAULT gen_random_uuid(),
 date bigint NOT NULL,
 ownerEmail text NOT NULL,
 company text NOT NULL,
 title text NOT NULL,
 description text NOT NULL,
 externalUrl text NOT NULL,
 remote boolean NOT NULL DEFAULT false,
 location text,
 salaryLo integer,
 salaryHi integer,
 currency text,
 country text,
 tags text[],
 image text,
 seniority text,
 other text[],
 active boolean NOT NULL DEFAULT false
);

ALTER TABLE jobs
ADD CONSTRAINT pk_jobs PRIMARY KEY (id);

CREATE TABLE users(
          id SERIAL,
          email VARCHAR(255) NOT NULL UNIQUE,
          hashedPassword text NOT NULL,
          firstName text NOT NULL,
          lastName text NOT NULL,
          company text NOT NULL,
          role text NOT NULL
);

ALTER TABLE users
ADD CONSTRAINT pk_users PRIMARY KEY (id);
