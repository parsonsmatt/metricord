-- The initial database state for the Metricord application.

CREATE TABLE user (
    id      SERIAL,
    email   TEXT UNIQUE NOT NULL,
);

CREATE TABLE schema (
    id      SERIAL,
    creator INTEGER NOT NULL REFERENCES user (id)
    name    TEXT NOT NULL,
);

CREATE TABLE permission (
    id      SERIAL,
    user    INTEGER NOT NULL REFERENCES user (id),
    schema  INTEGER NOT NULL REFERENCES schema (id),
    role    TEXT NOT NULL
);

CREATE TABLE metric (
    id          BIGSERIAL,
    schema      INTEGER NOT NULL REFERENCES schema (id),
    timestamp   TIMESTAMP NOT NULL,
    value       DOUBLE PRECISION NOT NULL
);

CREATE TABLE migration (
    id  SERIAL,
    filename TEXT NOT NULL UNIQUE,
    run TIMESTAMP NOT NULL
);
