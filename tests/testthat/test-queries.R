ALL_QUERIES <- c("kpis", "enrollment_by_phase", "sites_per_trial", "duration_by_phase",
                 "termination_rate", "stop_reasons", "top_countries", "trials_table")

with_fixture_db <- function(code) {
  path <- build_fixture_db()
  con <- open_db(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  code(con)
}

test_that("the fixture pipeline builds a database and every query runs", {
  with_fixture_db(function(con) {
    apply_filters(con, select_everything(con))
    for (q in ALL_QUERIES) expect_s3_class(run_query(con, q), "data.frame")
  })
})

test_that("headline numbers match hand counts on the fixture", {
  with_fixture_db(function(con) {
    apply_filters(con, select_everything(con))
    k <- run_query(con, "kpis")
    expect_equal(k$n_trials, 6)
    # Final status: 001 completed, 002 terminated, 004 withdrawn, 006 completed.
    # 003 is recruiting and 005 is UNKNOWN, so neither is in the denominator.
    expect_equal(k$n_final, 4)
    expect_equal(k$n_stopped, 2)
    expect_equal(k$stopped_early_rate, 0.5)
    # Actual, non-zero enrollment: 4, 40, 300. 003 is ESTIMATED; 004 enrolled 0.
    expect_equal(k$median_enrollment, 40)
    # Trials listing sites: 3, 1, 1.
    expect_equal(k$median_sites, 1)
    # Completed with both dates ACTUAL: 731 days (001) and 182 days (006).
    expect_equal(k$median_duration_months, mean(c(731, 182)) / (365.25 / 12))
  })
})

test_that("termination rate excludes ongoing and UNKNOWN trials", {
  with_fixture_db(function(con) {
    apply_filters(con, select_everything(con))
    r <- run_query(con, "termination_rate")
    expect_equal(sum(r$n_final[r$area == "oncology"]), 4)
    expect_false("Not reported" %in% r$phase)  # 005, the only such trial, is UNKNOWN
    cv <- r[r$area == "cardiovascular", ]
    expect_equal(cv$n_final, 1)
    expect_equal(cv$stopped_early_rate, 0)
  })
})

test_that("selecting one area also restricts the area links", {
  with_fixture_db(function(con) {
    apply_filters(con, modifyList(select_everything(con), list(areas = "oncology")))
    expect_equal(run_query(con, "kpis")$n_trials, 5)  # 005 is cardiovascular only
    # 006 is in both areas, but with only oncology selected it must not appear under
    # cardiovascular.
    expect_equal(unique(run_query(con, "termination_rate")$area), "oncology")
  })
})

test_that("the country breakdown counts trials, not sites", {
  with_fixture_db(function(con) {
    apply_filters(con, select_everything(con))
    countries <- run_query(con, "top_countries")
    n <- setNames(countries$n_trials, countries$country)
    # 001 lists two US sites and 002 one: two trials, not three sites.
    expect_equal(n[["United States"]], 2)
    expect_equal(n[["France"]], 1)
  })
})

test_that("an empty selection returns zeros rather than NULLs or errors", {
  with_fixture_db(function(con) {
    apply_filters(con, modifyList(select_everything(con), list(phases = character())))
    k <- run_query(con, "kpis")
    expect_equal(k$n_trials, 0)
    expect_equal(k$n_stopped, 0)
    expect_true(is.na(k$stopped_early_rate))
    for (q in ALL_QUERIES[-1]) expect_equal(nrow(run_query(con, q)), 0)
  })
})

test_that("filter values are bound as data, never spliced into SQL", {
  with_fixture_db(function(con) {
    hostile <- "Phase 3'); DROP TABLE studies; --"
    apply_filters(con, modifyList(select_everything(con), list(phases = c("Phase 3", hostile))))
    expect_equal(run_query(con, "kpis")$n_trials, 1)
    expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM studies")$n, 6)
  })
})

test_that("a consistent build passes validation", {
  path <- build_fixture_db()
  # Fixture: five unique oncology studies (one repeated across pages), two cardiovascular.
  expect_true(validate_database(path, c(oncology = 5L, cardiovascular = 2L)))
})

test_that("validation fails when the stored studies don't match the API's count", {
  path <- build_fixture_db()
  expect_error(validate_database(path, c(oncology = 6L, cardiovascular = 2L)),
               "oncology: 5 studies linked, but the API reported 6")
  expect_error(validate_database(path, c(oncology = 5L)), "area that wasn't extracted")
})

test_that("SQL medians and quartiles match R's on random data", {
  set.seed(42)
  sizes <- c("Phase 1" = 7, "Phase 2" = 10, "Phase 3" = 1, "Phase 4" = 2)
  phase <- rep(names(sizes), sizes)
  enrollment <- sample(1:5000, length(phase))
  studies <- make_studies(
    nct_id = sprintf("NCT%08d", seq_along(phase)), phase = phase, enrollment = enrollment
  )
  con <- open_db(build_synthetic_db(studies))
  on.exit(DBI::dbDisconnect(con))
  apply_filters(con, select_everything(con))
  r <- run_query(con, "enrollment_by_phase")
  for (p in names(sizes)) {
    x <- enrollment[phase == p]
    row <- r[r$phase == p, ]
    expect_equal(row$n_trials, length(x))
    expect_equal(row$median, median(x))
    expect_equal(row$p25, unname(quantile(x, 0.25, type = 1)))
    expect_equal(row$p75, unname(quantile(x, 0.75, type = 1)))
  }
})

test_that("ESTIMATED and zero enrollment are left out of enrollment statistics", {
  studies <- make_studies(
    nct_id = sprintf("NCT%08d", 1:4),
    enrollment = c(10L, 1000L, 0L, 30L),
    enrollment_type = c("ACTUAL", "ESTIMATED", "ACTUAL", "ACTUAL")
  )
  con <- open_db(build_synthetic_db(studies))
  on.exit(DBI::dbDisconnect(con))
  apply_filters(con, select_everything(con))
  r <- run_query(con, "enrollment_by_phase")
  expect_equal(r$n_trials, 2)
  expect_equal(r$median, 20)
})

test_that("completed and terminated durations are reported separately", {
  studies <- make_studies(
    nct_id = sprintf("NCT%08d", 1:4),
    overall_status = c("COMPLETED", "COMPLETED", "TERMINATED", "RECRUITING"),
    duration_months = c(24, 36, 6, 12)
  )
  con <- open_db(build_synthetic_db(studies))
  on.exit(DBI::dbDisconnect(con))
  apply_filters(con, select_everything(con))
  r <- run_query(con, "duration_by_phase")
  expect_equal(r$median[r$outcome == "Completed"], 30)
  expect_equal(r$median[r$outcome == "Terminated"], 6)
  expect_equal(sum(r$n_trials), 3)  # the recruiting trial is excluded
})
