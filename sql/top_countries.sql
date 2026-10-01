-- Countries hosting the most trials in the current selection, the first question in
-- choosing where to place sites. A trial with several sites in one country counts once
-- for that country (study_countries is already distinct per trial).
SELECT c.country,
       COUNT(*) AS n_trials
FROM study_countries AS c
JOIN f ON f.nct_id = c.nct_id
GROUP BY c.country
ORDER BY n_trials DESC
LIMIT 15
