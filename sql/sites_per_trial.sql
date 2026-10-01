-- How many sites trials use, as the share of each phase's trials in ordinal buckets.
--
-- Trials listing no locations are excluded rather than counted as zero-site trials: an
-- empty location list usually means sites were never registered, not that there were
-- none. Five buckets, because five is as many steps as the chart's single-hue ramp
-- can keep visibly distinct.
WITH bucketed AS (
  SELECT phase,
         CASE
           WHEN site_count = 1 THEN '1'
           WHEN site_count <= 10 THEN '2-10'
           WHEN site_count <= 50 THEN '11-50'
           WHEN site_count <= 100 THEN '51-100'
           ELSE '>100'
         END AS sites_bucket
  FROM f
  WHERE site_count > 0
)
SELECT phase,
       sites_bucket,
       COUNT(*) AS n_trials,
       CAST(COUNT(*) AS REAL) / SUM(COUNT(*)) OVER (PARTITION BY phase) AS share_of_phase
FROM bucketed
GROUP BY phase, sites_bucket
