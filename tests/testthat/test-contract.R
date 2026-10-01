# The pipeline writes values and the app interprets them. These tests fail when one side
# changes without the other.

test_that("every phase the pipeline can emit is one the app knows how to order", {
  emitted <- normalize_phase(c("EARLY_PHASE1", "PHASE1", "PHASE1|PHASE2", "PHASE2",
                               "PHASE2|PHASE3", "PHASE3", "PHASE4", "NA", NA, "PHASE1|PHASE3"))
  expect_true(all(emitted %in% PHASE_LEVELS))
})

test_that("the site buckets the SQL emits match the chart's ordinal scale", {
  studies <- make_studies(nct_id = sprintf("NCT%08d", 1:6), site_count = c(1L, 2L, 11L, 51L, 101L, 0L))
  con <- open_db(build_synthetic_db(studies))
  on.exit(DBI::dbDisconnect(con))
  apply_filters(con, select_everything(con))
  emitted <- run_query(con, "sites_per_trial")$sites_bucket
  expect_setequal(emitted, SITE_BUCKETS)
})

test_that("every registry status in the snapshot has a display label", {
  # The statuses ClinicalTrials.gov uses for interventional studies in this snapshot.
  registry <- c("COMPLETED", "UNKNOWN", "RECRUITING", "TERMINATED", "ACTIVE_NOT_RECRUITING",
                "WITHDRAWN", "NOT_YET_RECRUITING", "ENROLLING_BY_INVITATION", "SUSPENDED")
  expect_true(all(registry %in% names(STATUS_LABELS)))
})

test_that("every stop category has a place in the stop-reason chart", {
  categories <- c(names(STOP_RULES), "Other", "Not reported")
  d <- data.frame(category = categories, n_trials = seq_along(categories),
                  share = seq_along(categories) / sum(seq_along(categories)))
  p <- plot_stop_reasons(d)
  expect_setequal(levels(p$data$category), categories)
  # Catch-alls sit at the bottom whatever their size (factor levels run bottom to top).
  expect_equal(levels(p$data$category)[1:2], rev(STOP_CATEGORY_LAST))
})
