-- Months from study start to primary completion, by phase and outcome.
--
-- Only trials whose start and primary completion dates are both ACTUAL are included
-- (duration_months is NULL otherwise; see pipeline/clean.R). Completed and terminated
-- trials are kept apart: a terminated trial's "completion" is the date it stopped, so
-- pooling them would understate how long a trial takes to finish.
WITH durations AS (
  SELECT phase,
         CASE overall_status WHEN 'COMPLETED' THEN 'Completed' ELSE 'Terminated' END AS outcome,
         duration_months AS v
  FROM f
  WHERE overall_status IN ('COMPLETED', 'TERMINATED')
    AND duration_months IS NOT NULL
),
ranked AS (
  SELECT phase, outcome, v,
         ROW_NUMBER() OVER (PARTITION BY phase, outcome ORDER BY v) AS rn,
         COUNT(*) OVER (PARTITION BY phase, outcome) AS n
  FROM durations
)
SELECT phase,
       outcome,
       MAX(n) AS n_trials,
       MAX(CASE WHEN rn = (n * 25 + 99) / 100 THEN v END) AS p25,
       AVG(CASE WHEN rn IN ((n + 1) / 2, (n + 2) / 2) THEN v END) AS median,
       MAX(CASE WHEN rn = (n * 75 + 99) / 100 THEN v END) AS p75
FROM ranked
GROUP BY phase, outcome
