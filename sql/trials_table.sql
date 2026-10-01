-- Row-level listing behind the searchable table, newest first.
SELECT f.nct_id,
       f.title,
       (SELECT GROUP_CONCAT(a.area, ', ') FROM study_areas AS a WHERE a.nct_id = f.nct_id) AS areas,
       f.phase,
       f.overall_status,
       f.start_year,
       f.enrollment,
       f.enrollment_type,
       f.site_count,
       ROUND(f.duration_months, 1) AS duration_months,
       f.sponsor,
       f.sponsor_class,
       f.stop_category
FROM f
ORDER BY f.start_date DESC, f.nct_id
