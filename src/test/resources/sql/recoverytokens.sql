CREATE TABLE recoverytokens (
    email text NOT NULL,
    token text NOT NULL,
    expiration bigint NOT NULL
);

ALTER TABLE recoverytokens
ADD CONSTRAINT pk_recoverytokens PRIMARY KEY (email);

INSERT INTO recoverytokens(
    email,
    token,
    expiration
) VALUES (
    'mario@company.com',
    'AABB1234',
    EXTRACT(EPOCH FROM CURRENT_TIMESTAMP) * 1000 + 60000
);