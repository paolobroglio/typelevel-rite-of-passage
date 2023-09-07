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

INSERT INTO users(
    id,
    email,
    hashedPassword,
    firstName,
    lastName,
    company,
    role
) VALUES (
    10,
    'john@company.com',
    '$2a$10$7Adagujg935jeLzl6vQyPuwy6eckdMivlWaJzXkyo8vGG38ldbb12',
    'John',
    'Doe',
    'Company SPA',
    'ADMIN'
);

INSERT INTO users(
    id,
    email,
    hashedPassword,
    firstName,
    lastName,
    company,
    role
) VALUES (
    11,
    'mario@company.com',
    '$2a$10$7Adagujg935jeLzl6vQyPuwy6eckdMivlWaJzXkyo8vGG38ldbb12',
    'Mario',
    'Rossi',
    'Company SPA',
    'RECRUITER'
);