-- Analysis schema.
--
-- One row per trial in studies. Therapeutic area is many-to-many (a cardio-oncology
-- trial belongs to both areas), so it lives in study_areas rather than as a column.
-- Dates are ISO-8601 text, SQLite's convention; *_precision records whether the day
-- was registered or imputed.

CREATE TABLE studies (
  nct_id                            TEXT PRIMARY KEY,
  title                             TEXT NOT NULL,
  overall_status                    TEXT NOT NULL,
  why_stopped                       TEXT,
  stop_category                     TEXT,
  phase                             TEXT NOT NULL,
  phase_raw                         TEXT,
  enrollment                        INTEGER,
  enrollment_type                   TEXT,
  start_date                        TEXT,
  start_date_precision              TEXT,
  start_date_type                   TEXT,
  start_year                        INTEGER,
  primary_completion_date           TEXT,
  primary_completion_date_precision TEXT,
  primary_completion_date_type      TEXT,
  duration_months                   REAL,
  sponsor                           TEXT,
  sponsor_class                     TEXT NOT NULL,
  site_count                        INTEGER NOT NULL
);

CREATE TABLE study_areas (
  nct_id TEXT NOT NULL REFERENCES studies (nct_id),
  area   TEXT NOT NULL,
  PRIMARY KEY (nct_id, area)
);

-- One row per listed location, exactly as registered.
CREATE TABLE sites (
  nct_id   TEXT NOT NULL REFERENCES studies (nct_id),
  facility TEXT,
  city     TEXT,
  state    TEXT,
  country  TEXT
);

-- Distinct countries per trial, derived from sites after the load (see
-- sql/derive.sql). A trial with forty US sites is one row here.
CREATE TABLE study_countries (
  nct_id  TEXT NOT NULL REFERENCES studies (nct_id),
  country TEXT NOT NULL,
  PRIMARY KEY (nct_id, country)
) WITHOUT ROWID;

-- Conditions as the sponsor entered them (free text, not MeSH-normalised).
CREATE TABLE conditions (
  nct_id    TEXT NOT NULL REFERENCES studies (nct_id),
  condition TEXT NOT NULL
);

-- Provenance for the snapshot: when each area was pulled, with what filter, and how
-- many studies the API reported.
CREATE TABLE snapshot (
  key   TEXT PRIMARY KEY,
  value TEXT NOT NULL
);

CREATE INDEX ix_studies_phase_status_year ON studies (phase, overall_status, start_year);
CREATE INDEX ix_study_areas_area ON study_areas (area, nct_id);
CREATE INDEX ix_conditions_nct_id ON conditions (nct_id);
-- sites is not indexed: the app never reads it directly (study_countries serves the
-- country breakdown), and an index on 900k rows would add about 20 MB to the file.
