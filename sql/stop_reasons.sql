-- Why trials stopped early, by category. Categories come from the rule-based
-- categorize_stop_reason() in pipeline/clean.R, which is scored against a hand-labelled
-- validation set in the tests. "Not reported" means the registry has no reason text.
SELECT stop_category AS category,
       COUNT(*) AS n_trials,
       CAST(COUNT(*) AS REAL) / SUM(COUNT(*)) OVER () AS share
FROM f
WHERE overall_status IN ('TERMINATED', 'WITHDRAWN')
GROUP BY stop_category
ORDER BY n_trials DESC
