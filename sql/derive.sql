-- Derived tables, computed in SQL after the base tables are loaded.

-- One row per (trial, country). The country breakdown counts trials, not sites, so
-- de-duplicating once here saves a COUNT(DISTINCT) over every site on every filter
-- change.
INSERT INTO study_countries (nct_id, country)
SELECT DISTINCT nct_id, country
FROM sites
WHERE country IS NOT NULL;
