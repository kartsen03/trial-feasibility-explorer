-- The dashboard filter. apply_filters() materialises this as the temp table f, which
-- every other query reads, so the filter is defined in exactly one place.
--
-- Each multi-select arrives as a single JSON-array parameter and is expanded with
-- json_each(), which keeps the statement fully parameterised however many values are
-- selected. An empty selection is '[]' and matches nothing.
--
-- Only the columns some query reads are copied; this runs on every filter change.
SELECT s.nct_id, s.title, s.phase, s.overall_status, s.start_date, s.start_year,
       s.enrollment, s.enrollment_type, s.site_count, s.duration_months,
       s.sponsor, s.sponsor_class, s.stop_category
FROM studies AS s
WHERE s.phase IN (SELECT value FROM json_each(:phases))
  AND s.overall_status IN (SELECT value FROM json_each(:statuses))
  AND s.sponsor_class IN (SELECT value FROM json_each(:sponsor_classes))
  AND s.start_year BETWEEN :year_min AND :year_max
  -- Every study belongs to at least one area, so with all areas selected the check is
  -- always true. :all_areas lets SQLite short-circuit past the per-row subquery, which
  -- is the most expensive part of this statement.
  AND (:all_areas OR EXISTS (
    SELECT 1
    FROM study_areas AS a
    WHERE a.nct_id = s.nct_id
      AND a.area IN (SELECT value FROM json_each(:areas))
  ))
