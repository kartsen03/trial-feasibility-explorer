-- Headline numbers for the current filter.
--
-- SQLite has no MEDIAN, so each median averages the middle one or two values of the
-- ordered set: rows (n + 1) / 2 and (n + 2) / 2 under integer division, which are the
-- same row when n is odd. Each statistic uses the population it is meaningful for; see
-- the per-chart queries for why.
WITH
enrolled AS (
  SELECT enrollment AS v,
         ROW_NUMBER() OVER (ORDER BY enrollment) AS rn,
         COUNT(*) OVER () AS n
  FROM f
  WHERE enrollment_type = 'ACTUAL' AND enrollment > 0
),
sited AS (
  SELECT site_count AS v,
         ROW_NUMBER() OVER (ORDER BY site_count) AS rn,
         COUNT(*) OVER () AS n
  FROM f
  WHERE site_count > 0
),
completed AS (
  SELECT duration_months AS v,
         ROW_NUMBER() OVER (ORDER BY duration_months) AS rn,
         COUNT(*) OVER () AS n
  FROM f
  WHERE overall_status = 'COMPLETED' AND duration_months IS NOT NULL
),
finished AS (
  -- SUM over zero rows is NULL, not 0.
  SELECT COUNT(*) AS n_final,
         COALESCE(SUM(overall_status IN ('TERMINATED', 'WITHDRAWN')), 0) AS n_stopped
  FROM f
  WHERE overall_status IN ('COMPLETED', 'TERMINATED', 'WITHDRAWN')
)
SELECT
  (SELECT COUNT(*) FROM f) AS n_trials,
  (SELECT AVG(v) FROM enrolled WHERE rn IN ((n + 1) / 2, (n + 2) / 2)) AS median_enrollment,
  (SELECT AVG(v) FROM sited WHERE rn IN ((n + 1) / 2, (n + 2) / 2)) AS median_sites,
  (SELECT AVG(v) FROM completed WHERE rn IN ((n + 1) / 2, (n + 2) / 2)) AS median_duration_months,
  n_final,
  n_stopped,
  CAST(n_stopped AS REAL) / NULLIF(n_final, 0) AS stopped_early_rate
FROM finished
