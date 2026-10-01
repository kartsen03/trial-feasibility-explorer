read_page <- function(...) jsonlite::read_json(fixture("raw", ...), simplifyVector = FALSE)

test_that("raw values are kept and absent fields become NA", {
  s <- parse_page(read_page("oncology", "page_0001.json"))$studies
  expect_equal(s$nct_id, c("NCT90000001", "NCT90000002", "NCT90000003"))
  expect_equal(s$phase_raw, c("PHASE3", "PHASE1|PHASE2", "PHASE2"))
  # whyStopped is absent from the JSON for trials that weren't stopped, not null.
  expect_equal(s$why_stopped, c(NA, "Low Accrual", NA))
  expect_equal(s$start_date_raw[[2]], "2012-08")
  expect_equal(s$enrollment_type[[3]], "ESTIMATED")
})

test_that("site_count is each study's row count in sites, duplicate listings included", {
  t <- parse_page(read_page("oncology", "page_0001.json"))
  expect_equal(t$studies$site_count, c(3L, 1L, 0L))
  rows <- as.vector(table(factor(t$sites$nct_id, levels = t$studies$nct_id)))
  expect_equal(rows, t$studies$site_count)
})

test_that("a sparse record parses to NAs rather than failing", {
  t <- parse_page(read_page("cardiovascular", "page_0001.json"))
  sparse <- t$studies[t$studies$nct_id == "NCT90000005", ]
  expect_true(is.na(sparse$phase_raw))
  expect_true(is.na(sparse$enrollment))
  expect_true(is.na(sparse$primary_completion_date_raw))
  expect_true(is.na(sparse$start_date_type))
  expect_equal(sparse$site_count, 0L)
  expect_false("NCT90000005" %in% t$conditions$nct_id)
})

test_that("an empty page gives empty tables with the usual columns", {
  t <- parse_page(list(studies = list()))
  expect_equal(nrow(t$studies), 0)
  expect_equal(nrow(t$sites), 0)
  expect_equal(nrow(t$conditions), 0)
  expect_true(all(c("nct_id", "phase_raw", "site_count") %in% names(t$studies)))
})

test_that("a study repeated on a later page is kept once, without doubling its sites", {
  a <- parse_area_dir(fixture("raw", "oncology"))
  expect_equal(a$n_repeats, 1L)
  expect_equal(sum(a$studies$nct_id == "NCT90000002"), 1L)
  expect_equal(sum(a$sites$nct_id == "NCT90000002"), 1L)
})

test_that("a trial in both areas is one study with two area links", {
  combined <- combine_areas(list(
    oncology = parse_area_dir(fixture("raw", "oncology")),
    cardiovascular = parse_area_dir(fixture("raw", "cardiovascular"))
  ))
  expect_equal(nrow(combined$studies), 6L)
  expect_equal(anyDuplicated(combined$studies$nct_id), 0L)
  links <- combined$study_areas$area[combined$study_areas$nct_id == "NCT90000006"]
  expect_setequal(links, c("oncology", "cardiovascular"))
  expect_equal(sum(combined$sites$nct_id == "NCT90000006"), 1L)
  expect_equal(sum(combined$conditions$nct_id == "NCT90000006"), 2L)
})
