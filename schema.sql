-- cellar database schema (v4)
--
-- To create a fresh database:
--   sqlite3 inventory.db < schema.sql

PRAGMA foreign_keys = ON;

CREATE TABLE producers(
  pid INTEGER PRIMARY KEY AUTOINCREMENT,
  producer TEXT NOT NULL,
  varietal TEXT NOT NULL,
  vineyard TEXT DEFAULT NULL,
  designation TEXT DEFAULT NULL,
  subregion TEXT DEFAULT NULL,
  region TEXT NOT NULL,
  country TEXT NOT NULL,
  category TEXT DEFAULT NULL,
  created_at TEXT NOT NULL DEFAULT (datetime('now')));

CREATE TABLE inventory(
  iid INTEGER PRIMARY KEY AUTOINCREMENT,
  pid INTEGER NOT NULL,
  vint INTEGER,
  btl INTEGER DEFAULT 750 NOT NULL,
  oi INTEGER DEFAULT 0 NOT NULL,
  hi INTEGER DEFAULT 0 NOT NULL,
  ci INTEGER DEFAULT 0 NOT NULL,
  alc REAL DEFAULT NULL,
  w0 INTEGER DEFAULT NULL,
  w1 INTEGER DEFAULT NULL,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  FOREIGN KEY (pid) REFERENCES producers(pid) ON DELETE RESTRICT,
  CHECK (oi >= 0), CHECK (hi >= 0), CHECK (ci >= 0), CHECK (btl > 0),
  CHECK (alc IS NULL OR (alc > 0.0 AND alc <= 100.0)),
  CHECK (w0 IS NULL OR (w0 >= 2000 AND w0 <= 2100)),
  CHECK (w1 IS NULL OR (w1 >= 2000 AND w1 <= 2100)),
  CHECK (w1 IS NULL OR w0 IS NULL OR w1 >= w0));

CREATE TABLE notes(
  nid INTEGER PRIMARY KEY AUTOINCREMENT,
  iid INTEGER NOT NULL,
  note TEXT NOT NULL,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  FOREIGN KEY (iid) REFERENCES inventory(iid) ON DELETE CASCADE);

CREATE TABLE vineyards(
  vid INTEGER PRIMARY KEY AUTOINCREMENT,
  region TEXT NOT NULL,
  village TEXT NOT NULL,
  vineyard TEXT NOT NULL,
  umbrella_vin TEXT DEFAULT NULL,
  designation TEXT DEFAULT NULL,
  note TEXT DEFAULT NULL);

CREATE VIEW wines AS
SELECT inventory.iid,
       inventory.pid,
       inventory.vint,
       inventory.btl,
       inventory.oi,
       inventory.hi,
       inventory.ci,
       inventory.oi + inventory.hi AS tot,
       inventory.alc,
       inventory.w0,
       inventory.w1,
       producers.producer,
       producers.varietal,
       producers.vineyard,
       producers.designation,
       producers.subregion,
       producers.region,
       producers.country,
       producers.category
FROM inventory
LEFT JOIN producers ON inventory.pid = producers.pid
ORDER BY producer;

CREATE INDEX idx_producers_country ON producers(country);
CREATE INDEX idx_producers_region ON producers(region);
CREATE INDEX idx_producers_producer ON producers(producer);
CREATE INDEX idx_inventory_pid ON inventory(pid);

-- Uniqueness uses IFNULL because SQLite treats NULLs as distinct in
-- UNIQUE constraints, which would let duplicates of NULL-valued rows
-- (e.g. no vineyard, or a non-vintage wine) through.
CREATE UNIQUE INDEX idx_producers_unique ON producers(
  producer, varietal, IFNULL(vineyard, ''), IFNULL(designation, ''),
  IFNULL(category, ''));
CREATE UNIQUE INDEX idx_inventory_unique ON inventory(pid, IFNULL(vint, 0), btl);

CREATE TABLE tastings(
  tid        INTEGER PRIMARY KEY AUTOINCREMENT,
  iid        INTEGER NOT NULL,
  date       TEXT NOT NULL,
  appearance INTEGER,
  aroma      INTEGER,
  taste      INTEGER,
  aftertaste INTEGER,
  impression INTEGER,
  score      INTEGER GENERATED ALWAYS AS
               (appearance+aroma+taste+aftertaste+impression) STORED,
  filename   TEXT NOT NULL UNIQUE,
  FOREIGN KEY (iid) REFERENCES inventory(iid) ON DELETE RESTRICT
);

CREATE INDEX idx_notes_iid ON notes(iid);
CREATE INDEX idx_tastings_iid ON tastings(iid);
