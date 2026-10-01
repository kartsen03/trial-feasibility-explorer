-- Area links for the filtered trials, restricted to the selected areas. Materialised as
-- the temp table f_areas. Without the restriction, a cardio-oncology trial would show
-- up under cardiovascular even when only oncology is selected.
SELECT a.nct_id, a.area
FROM study_areas AS a
JOIN f ON f.nct_id = a.nct_id
WHERE a.area IN (SELECT value FROM json_each(:areas))
