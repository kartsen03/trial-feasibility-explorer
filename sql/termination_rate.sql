-- How often trials stop early, by therapeutic area and phase.
--
-- The denominator is trials that reached a final status: completed, terminated or
-- withdrawn. Ongoing trials haven't had the chance to stop, and UNKNOWN-status trials
-- (not verified by the sponsor in two or more years) have no reliable outcome, so both
-- are excluded. A trial in both areas counts once in each area's rate.
SELECT a.area,
       f.phase,
       COUNT(*) AS n_final,
       SUM(f.overall_status = 'TERMINATED') AS n_terminated,
       SUM(f.overall_status = 'WITHDRAWN') AS n_withdrawn,
       CAST(SUM(f.overall_status IN ('TERMINATED', 'WITHDRAWN')) AS REAL) / COUNT(*) AS stopped_early_rate
FROM f
JOIN f_areas AS a ON a.nct_id = f.nct_id
WHERE f.overall_status IN ('COMPLETED', 'TERMINATED', 'WITHDRAWN')
GROUP BY a.area, f.phase
