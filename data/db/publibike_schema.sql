CREATE TYPE GENDER AS ENUM ('M', 'F');
CREATE TYPE DIRECTION AS ENUM ('IN', 'OUT');


-- BINCITTA DATA
CREATE SCHEMA bicincitta;

CREATE TABLE bicincitta.subnetworks (
  id            INTEGER NOT NULL PRIMARY KEY,
  name          VARCHAR(128) NOT NULL UNIQUE
);

CREATE TABLE bicincitta.stations (
  id            INTEGER NOT NULL PRIMARY KEY,
  name          VARCHAR(128) NOT NULL UNIQUE,
  subnetwork_id INTEGER NOT NULL REFERENCES bicincitta.subnetworks (id),
  latitude      NUMERIC NOT NULL,
  longitude     NUMERIC NOT NULL
);
CREATE TABLE bicincitta.users (
  id            INTEGER NOT NULL PRIMARY KEY,
  subnetwork_id INTEGER NOT NULL REFERENCES bicincitta.subnetworks (id),
  gender        GENDER NOT NULL,
  postal_code   VARCHAR(16),
  address       VARCHAR(256),
  expires       DATE
);

CREATE TABLE bicincitta.transactions (
  id            BIGINT NOT NULL PRIMARY KEY,
  timestamp     TIMESTAMP WITH TIME ZONE NOT NULL,
  user_id       INTEGER NOT NULL REFERENCES bicincitta.users (id),
  station_id    INTEGER NOT NULL REFERENCES bicincitta.stations (id),
  direction     DIRECTION NOT NULL
);


-- SyCube

-- ???


-- Common

CREATE SCHEMA data;

CREATE TABLE data.communes (
	id	      INTEGER NOT NULL PRIMARY KEY,
	name 	      VARCHAR(128) NOT NULL UNIQUE,
	canton	      VARCHAR(128) NOT NULL
)
SELECT AddGeometryColumn('data', 'communes', 'boundary', 'MULTIPOLYGON', 2);

CREATE TABLE data.aggloes (
	id	      INTEGER NOT NULL PRIMARY KEY
);
SELECT AddGeometryColumn('data', 'aggloes', 'buffer', 'POLYGON', 2);
SELECT AddGeometryColumn('data', 'aggloes', 'boundary', 'MULTIPOLYGON', 2);

CREATE TABLE data.map_commune_to_agglo(
	commune_id    INTEGER NOT NULL REFERENCES data.communes (id) PRIMARY KEY,
	agglo_id      INTEGER NOT NULL REFERENCES data.aggloes (id)
)

CREATE TABLE data.networks (
  id                  INTEGER NOT NULL PRIMARY KEY,
  agglo_id            INTEGER NOT NULL REFERENCES data.aggloes (id),
  name                VARCHAR(128) NOT NULL UNIQUE
);

CREATE TABLE data.stations (
  id                  INTEGER NOT NULL PRIMARY KEY,
  name                VARCHAR(128) NOT NULL UNIQUE,
  network_id          INTEGER NOT NULL REFERENCES data.networks (id)
);
SELECT AddGeometryColumn('data', 'stations', 'position', 21781, 'POINT', 3);

CREATE TABLE data.users (
  id                  INTEGER NOT NULL PRIMARY KEY,
  network_id          INTEGER NOT NULL REFERENCES data.networks (id),
  gender              GENDER,
  postal_code         VARCHAR(16),
  address             VARCHAR(256),
  created             DATE,
  expires             DATE
);

CREATE TABLE data.transactions (
  id                  BIGINT NOT NULL PRIMARY KEY,
  timestamp           TIMESTAMP WITH TIME ZONE NOT NULL,
  user_id             INTEGER NOT NULL REFERENCES bicincitta.users (id),
  station_id          INTEGER NOT NULL REFERENCES bicincitta.stations (id),
  direction           DIRECTION NOT NULL
);

CREATE TABLE data.trips (
  out_tx              BIGINT NOT NULL REFERENCES data.transactions (id),
  in_tx               BIGINT NOT NULL REFERENCES data.transactions (id),

  PRIMARY KEY (out_tx, in_tx)
);


-- VIEWS

CREATE SCHEMA views;

CREATE VIEW views.stations AS (
  SELECT
    s.id          AS id,
    s.name        AS name,
    s.network_id  AS network_id,
    n.name        AS network_name
  FROM data.stations AS s
  JOIN data.networks AS n ON n.id = s.network_id
);

CREATE VIEW views.users AS (
  SELECT
    u.id          AS id,
    u.gender      AS gender,
    u.postal_code AS postal_code,
    u.address     AS address,
    u.created     AS created,
    u.expires     AS expires,
    u.network_id  AS network_id,
    n.name        AS network_name
  FROM data.users     AS u
  JOIN data.networks  AS n ON n.id = u.network_id
);

CREATE VIEW views.transactions AS (
  SELECT
    t.id            AS id,
    t.timestamp     AS timestamp,
    t.direction     AS direction,
    t.station_id    AS station_id,
    s.name          AS station_name,
    s.network_id    AS network_id,
    s.network_name  AS network_name,
    t.user_id       AS user_id,
    u.gender        AS user_gender,
    u.postal_code   AS user_postal_code,
    u.address       AS user_address,
    u.created       AS user_created,
    u.expires       AS user_expires,
    u.network_id    AS user_network_id,
    u.network_name  AS user_network_name
  FROM data.transactions  AS t
  JOIN views.stations     AS s ON s.id = t.station_id
  JOIN views.users        AS u ON u.id = t.user_id
);

CREATE VIEW data.trips_view AS (
  SELECT
    t_out.id                            AS start_id,
    t_out.timestamp                     AS start_timestamp,
    t_out.station_id                    AS start_station_id,
    t_out.station_name                  AS start_station_name,
    t_out.network_id                    AS start_network_id,
    t_out.network_name                  AS start_network_name,
    t_in.id                             AS end_id,
    t_in.timestamp                      AS end_timestamp,
    t_in.station_id                     AS end_station_id,
    t_in.station_name                   AS end_station_name,
    t_in.network_id                     AS end_network_id,
    t_in.network_name                   AS end_network_name,
    (t_out.timestamp - t_in.timestamp)  AS duration,
    t_out.user_id                       AS user_id,
    t_out.user_gender                   AS user_gender,
    t_out.user_postal_code              AS user_postal_code,
    t_out.user_address                  AS user_address,
    t_out.user_created                  AS user_created,
    t_out.user_expires                  AS user_expires,
    t_out.user_network_id               AS user_network_id,
    t_out.user_network_name		AS user_network_name
  FROM data.trips           AS t
  JOIN views.transactions   AS t_out ON t_out.id = t.out_tx
  JOIN views.transactions   AS t_in  ON t_in.id = t.in_tx
);
