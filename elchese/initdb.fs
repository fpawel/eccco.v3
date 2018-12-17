﻿module initdb

let sql = """
PRAGMA foreign_keys = ON;
PRAGMA encoding = 'UTF-8';
--E:\Program Data\Аналитприбор\elco\elco.sqlite
--C:\Users\fpawel\AppData\Roaming\Аналитприбор\elco\elco.sqlite

CREATE TABLE IF NOT EXISTS gas
(
  gas_name TEXT PRIMARY KEY NOT NULL,
  code     INTEGER UNIQUE   NOT NULL
);

CREATE TABLE IF NOT EXISTS units
(
  units_name TEXT PRIMARY KEY NOT NULL,
  code       INTEGER UNIQUE   NOT NULL
);

CREATE TABLE IF NOT EXISTS product_type
(
  product_type_name   TEXT PRIMARY KEY NOT NULL,
  display_name        TEXT,
  gas_name            TEXT             NOT NULL,
  units_name          TEXT             NOT NULL,
  scale               REAL             NOT NULL,
  noble_metal_content REAL             NOT NULL,
  lifetime_months     INTEGER          NOT NULL CHECK (lifetime_months > 0),
  lc64                BOOLEAN          NOT NULL
    CONSTRAINT boolean_lc64
      CHECK (lc64 IN (0, 1)),
  points_method       INTEGER          NOT NULL
    CONSTRAINT points_method_2_or_3
      CHECK (points_method IN (2, 3)),

  FOREIGN KEY (gas_name) REFERENCES gas (gas_name),
  FOREIGN KEY (units_name) REFERENCES units (units_name)
);

CREATE TABLE IF NOT EXISTS party
(
  party_id           INTEGER PRIMARY KEY NOT NULL,
  old_party_id       TEXT,
  created_at         TIMESTAMP           NOT NULL DEFAULT (datetime('now')),
  updated_at         TIMESTAMP           NOT NULL DEFAULT (datetime('now')),
  product_type_name  TEXT                NOT NULL DEFAULT '035',
  concentration1     REAL                NOT NULL DEFAULT 0 CHECK (concentration1 >= 0),
  concentration2     REAL                NOT NULL DEFAULT 50 CHECK (concentration2 >= 0),
  concentration3     REAL                NOT NULL DEFAULT 100 CHECK (concentration3 >= 0),
  note               TEXT,

  min_fon            REAL                         DEFAULT -1,
  max_fon            REAL                         DEFAULT 2,
  max_d_fon          REAL                         DEFAULT 3,
  min_k_sens20       REAL                         DEFAULT 0.08,
  max_k_sens20       REAL                         DEFAULT 0.335,
  min_d_temp         REAL                         DEFAULT 0,
  max_d_temp         REAL                         DEFAULT 3,
  min_k_sens50       REAL                         DEFAULT 110,
  max_k_sens50       REAL                         DEFAULT 150,
  max_d_not_measured REAL                         DEFAULT 5,

  FOREIGN KEY (product_type_name) REFERENCES product_type (product_type_name)
);

CREATE TABLE IF NOT EXISTS product
(
  product_id        INTEGER PRIMARY KEY NOT NULL,
  party_id          INTEGER             NOT NULL,
  serial            INTEGER CHECK ( serial ISNULL OR serial > 0 ),
  place             INTEGER             NOT NULL CHECK (place >= 0),
  product_type_name TEXT,
  note              TEXT,

  i_f_minus20       REAL,
  i_f_plus20        REAL,
  i_f_plus50        REAL,

  i_s_minus20       REAL,
  i_s_plus20        REAL,
  i_s_plus50        REAL,

  i13               REAL,
  i24               REAL,
  i35               REAL,
  i26               REAL,
  i17               REAL,
  not_measured      REAL,
  firmware          BLOB,
  production        BOOLEAN             NOT NULL CHECK (production IN (0, 1)) DEFAULT 0,

  old_product_id    TEXT,
  old_serial        INTEGER,

  CONSTRAINT unique_party_place UNIQUE (party_id, place),
  CONSTRAINT unique_party_serial UNIQUE (party_id, serial),

  FOREIGN KEY (product_type_name) REFERENCES product_type (product_type_name),
  FOREIGN KEY (party_id) REFERENCES party (party_id)
    ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS product_temperature_current_k_sens
(
  product_id  INTEGER NOT NULL,
  temperature REAL    NOT NULL,
  current     REAL,
  k_sens      REAL,
  CONSTRAINT product_temperature_current_k_sens_primary_key UNIQUE (product_id, temperature),
  FOREIGN KEY (product_id) REFERENCES product (product_id)
    ON DELETE CASCADE
);

CREATE TRIGGER IF NOT EXISTS trigger_product_party_updated_at
  AFTER INSERT
  ON product
  BEGIN
    UPDATE party
    SET updated_at = datetime('now')
    WHERE party.party_id = new.party_id;
  END;

CREATE TRIGGER IF NOT EXISTS trigger_party_updated_at
  BEFORE UPDATE
  ON party
  BEGIN
    UPDATE party SET updated_at = (datetime('now')) WHERE party_id = old.party_id;
  END;

CREATE VIEW IF NOT EXISTS last_party AS
SELECT *
FROM party
ORDER BY created_at DESC
LIMIT 1;


CREATE VIEW IF NOT EXISTS party_info AS
SELECT *,
       cast(strftime('%Y', DATETIME(created_at, '+3 hours')) AS INTEGER) AS year,
       cast(strftime('%m', DATETIME(created_at, '+3 hours')) AS INTEGER) AS month,
       cast(strftime('%d', DATETIME(created_at, '+3 hours')) AS INTEGER) AS day,
       party_id IN (SELECT party_id FROM last_party)                     AS last
FROM party;

CREATE VIEW IF NOT EXISTS product_info_1 AS
SELECT product_id,
       product.party_id,
       created_at,
       serial,
       place,
       production,
       (CASE (product.product_type_name ISNULL)
          WHEN 1 THEN party.product_type_name
          WHEN 0
            THEN product.product_type_name END)                                AS applied_product_type_name,
       product.product_type_name                                               AS product_type_name,
       product.note                                                            AS note,
       round(i_f_minus20, 3)                                                   AS i_f_minus20,
       round(i_f_plus20, 3)                                                    AS i_f_plus20,
       round(i_f_plus50, 3)                                                    AS i_f_plus50,
       round(i_s_minus20, 3)                                                   AS i_s_minus20,
       round(i_s_plus20, 3)                                                    AS i_s_plus20,
       round(i_s_plus50, 3)                                                    AS i_s_plus50,
       round(i13, 3)                                                           AS i13,
       round(i24, 3)                                                           AS i24,
       round(i35, 3)                                                           AS i35,
       round(i26, 3)                                                           AS i26,
       round(i17, 3)                                                           AS i17,
       round(not_measured, 3)                                                  AS not_measured,

       round(100 * (i_s_plus50 - i_f_plus50) / (i_s_plus20 - i_f_plus20), 3)   AS k_sens50,
       round(100 * (i_s_minus20 - i_f_minus20) / (i_s_plus20 - i_f_plus20), 3) AS k_sens_minus20,

       round((i_s_plus20 - i_f_plus20) / (concentration3 - concentration1), 3) AS k_sens20,
       round(i13 - i_f_plus20, 3)                                              AS d_fon20,
       round(i_f_plus50 - i_f_plus20, 3)                                       AS d_fon50,
       round(not_measured - i_f_plus20, 3)                                     AS d_not_measured,
       (firmware NOT NULL AND LENGTH(firmware) > 0)                            AS has_firmware
FROM product
       INNER JOIN party ON party.party_id = product.party_id;

CREATE VIEW IF NOT EXISTS product_info_2 AS
SELECT q.*,
       max_d_not_measured ISNULL OR abs(d_not_measured) < max_d_not_measured AS ok_d_not_measured,
       max_d_temp ISNULL OR abs(d_fon50) < max_d_temp                        AS ok_d_fon50,
       max_d_fon ISNULL OR abs(d_fon20) < max_d_fon                          AS ok_d_fon20,
       min_k_sens20 ISNULL OR k_sens20 > min_k_sens20                        AS ok_min_k_sens20,
       max_k_sens20 ISNULL OR k_sens20 < max_k_sens20                        AS ok_max_k_sens20,
       min_k_sens50 ISNULL OR k_sens50 > min_k_sens50                        AS ok_min_k_sens50,
       max_k_sens50 ISNULL OR k_sens50 < max_k_sens50                        AS ok_max_k_sens50,
       i_f_plus20 < max_fon                                                  AS ok_fon20,
       gas.code                                                              AS gas_code,
       units.code                                                            AS units_code,
       gas.gas_name,
       units.units_name,
       scale,
       noble_metal_content,
       lifetime_months,
       lc64,
       points_method
FROM product_info_1 q
       INNER JOIN party ON party.party_id = q.party_id
       INNER JOIN product_type ON product_type.product_type_name = q.applied_product_type_name
       INNER JOIN gas ON product_type.gas_name = gas.gas_name
       INNER JOIN units ON product_type.units_name = units.units_name;

CREATE VIEW IF NOT EXISTS product_info AS
SELECT *,
       NOT ok_d_not_measured AND ok_d_fon50 AND ok_d_fon20 AND ok_min_k_sens20 AND ok_max_k_sens20 AND
       ok_min_k_sens50 AND ok_max_k_sens50 AND ok_fon20 AS ok
FROM product_info_2 q;


INSERT
  OR
REPLACE INTO units (units_name, code)
VALUES ('мг/м3', 2),
       ('ppm', 3),
       ('об. дол. %', 7),
       ('млн-1', 5);

INSERT
  OR
REPLACE INTO gas (gas_name, code)
VALUES ('CO', 0x11),
       ('H₂S', 0x22),
       ('NH₃', 0x33),
       ('Cl₂', 0x44),
       ('SO₂', 0x55),
       ('NO₂', 0x66),
       ('O₂', 0x88),
       ('NO', 0x99),
       ('HCl', 0xAA);

INSERT
  OR
REPLACE INTO product_type (product_type_name,
                           display_name,
                           gas_name,
                           units_name,
                           scale,
                           noble_metal_content,
                           lifetime_months,
                           lc64,
                           points_method)
VALUES ('035', '035', 'CO', 'мг/м3', 200, 0.1626, 18, 0, 3),
       ('035(2)', '035', 'CO', 'мг/м3', 200, 0.1456, 12, 0, 3),
       ('035-59', NULL, 'CO', 'об. дол. %', 0.5, 0.1891, 12, 0, 3),
       ('035-60', NULL, 'CO', 'мг/м3', 200, 0.1891, 12, 0, 3),
       ('035-61', NULL, 'CO', 'ppm', 2000, 0.1891, 12, 0, 3),
       ('035-80', NULL, 'CO', 'мг/м3', 200, 0.1456, 12, 0, 3),
       ('035-81', NULL, 'CO', 'мг/м3', 1500, 0.1456, 12, 0, 3),
       ('035-92', NULL, 'CO', 'об. дол. %', 0.5, 0.1891, 12, 0, 3),
       ('035-93', NULL, 'CO', 'млн-1', 200, 0.1891, 12, 0, 3),
       ('035-94', NULL, 'CO', 'млн-1', 2000, 0.1891, 12, 0, 3),
       ('035-105', NULL, 'CO', 'мг/м3', 200, 0.1456, 12, 0, 3),
       ('100', NULL, 'CO', 'мг/м3', 200, 0.0816, 12, 1, 3),
       ('100-05', NULL, 'CO', 'мг/м3', 50, 0.0816, 12, 1, 3),
       ('100-10', NULL, 'CO', 'мг/м3', 200, 0.0816, 12, 1, 3),
       ('100-15', NULL, 'CO', 'мг/м3', 50, 0.0816, 12, 1, 3),
       ('035-40', NULL, 'CO', 'мг/м3', 200, 0.1456, 12, 0, 2),
       ('035-21', NULL, 'CO', 'мг/м3', 200, 0.1456, 12, 0, 2),
       ('130-01', NULL, 'CO', 'мг/м3', 200, 0.1626, 12, 0, 3),
       ('035-70', NULL, 'CO', 'мг/м3', 200, 0.1626, 12, 0, 2),
       ('130-08', NULL, 'CO', 'ppm', 100, 0.1162, 12, 0, 3),
       ('035-117', NULL, 'NO₂', 'мг/м3', 200, 0.1626, 18, 1, 3),
       ('010-18', NULL, 'O₂', 'об. дол. %', 21, 0, 12, 1, 3),
       ('010-18', NULL, 'O₂', 'об. дол. %', 21, 0, 12, 1, 3),
       ('035-111', NULL, 'CO', 'мг/м3', 200, 0.1626, 12, 1, 3);


DELETE
FROM party
WHERE NOT EXISTS(SELECT product_id FROM product WHERE party.party_id = product.party_id);


"""
