-- Achieved enrollment by phase: the distribution a feasibility team benchmarks a new
-- study's enrollment target against.
--
-- Only ACTUAL counts are used. An ESTIMATED count is the sponsor's target, so mixing
-- the two would blend plans with outcomes. Trials with zero actual enrollment (withdrawn
-- before the first participant) are excluded.
--
-- Quartiles use the nearest-rank method: the value at rank ceil(p * n), written as
-- (n * 25 + 99) / 100 in integer arithmetic. The median averages the middle pair.
WITH ranked AS (
  SELECT phase,
         enrollment AS v,
         ROW_NUMBER() OVER (PARTITION BY phase ORDER BY enrollment) AS rn,
         COUNT(*) OVER (PARTITION BY phase) AS n
  FROM f
  WHERE enrollment_type = 'ACTUAL' AND enrollment > 0
)
SELECT phase,
       MAX(n) AS n_trials,
       MAX(CASE WHEN rn = (n * 25 + 99) / 100 THEN v END) AS p25,
       AVG(CASE WHEN rn IN ((n + 1) / 2, (n + 2) / 2) THEN v END) AS median,
       MAX(CASE WHEN rn = (n * 75 + 99) / 100 THEN v END) AS p75
FROM ranked
GROUP BY phase
