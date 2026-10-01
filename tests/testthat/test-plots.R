charts <- list(
  enrollment_by_phase = list(plot_enrollment, table_enrollment),
  sites_per_trial = list(plot_sites, table_sites),
  duration_by_phase = list(plot_duration, table_duration),
  termination_rate = list(plot_termination, table_termination),
  stop_reasons = list(plot_stop_reasons, table_stop_reasons),
  top_countries = list(plot_countries, table_countries)
)

test_that("every chart and table view builds from real query output", {
  path <- build_fixture_db()
  con <- open_db(path)
  on.exit(DBI::dbDisconnect(con))
  apply_filters(con, select_everything(con))
  for (q in names(charts)) {
    d <- run_query(con, q)
    tbl <- charts[[q]][[2]](d)
    expect_s3_class(tbl, "data.frame")
    if (q == "termination_rate") next  # the fixture is far below the minimum-n rule
    p <- charts[[q]][[1]](d)
    expect_s3_class(p, "ggplot")
    expect_s3_class(as_girafe(p, nrow(d)), "girafe")
  }
})

test_that("termination rates below the minimum number of finished trials aren't charted", {
  d <- data.frame(area = c("oncology", "oncology"), phase = c("Phase 2", "Phase 3"),
                  n_final = c(MIN_FINAL_FOR_RATE - 1, MIN_FINAL_FOR_RATE),
                  n_terminated = c(5, 2), n_withdrawn = c(0, 1), stopped_early_rate = c(0.26, 0.15))
  p <- plot_termination(d)
  expect_equal(as.character(p$data$phase), "Phase 3")
  expect_null(plot_termination(d[1, ]))
})

test_that("colour follows the area, not its rank", {
  d <- data.frame(area = "cardiovascular", phase = "Phase 3", n_final = 50,
                  n_terminated = 5, n_withdrawn = 1, stopped_early_rate = 0.12)
  built <- ggplot2::ggplot_build(plot_termination(d))
  expect_equal(built$data[[1]]$fill, unname(AREA_COLOURS[["cardiovascular"]]))
})

# Alphabetical tooltip order ("1 site", "11-50", "2-10", ...) disagrees with bucket
# order, so these fail if grouping ever falls back to the tooltip strings.
test_that("stacked segments run in bucket order and each label sits on its own segment", {
  share <- c(0.10, 0.20, 0.30, 0.15, 0.25)
  d <- data.frame(phase = "Phase 3", sites_bucket = SITE_BUCKETS, n_trials = 1:5,
                  share_of_phase = share)
  built <- ggplot2::ggplot_build(plot_sites(d))
  bars <- built$data[[1]]
  bars <- bars[order(bars$xmin), ]
  expect_equal(bars$fill, ORDINAL_5)
  # Each bucket's label must sit at the midpoint of the bar drawn in that bucket's colour.
  labels <- built$data[[2]]
  for (i in seq_along(SITE_BUCKETS)) {
    bar <- bars[bars$fill == ORDINAL_5[[i]], ]
    label_x <- labels$x[labels$label == fmt_pct(share[[i]], 0)]
    expect_equal(label_x, (bar$xmin + bar$xmax) / 2)
  }
})

test_that("dodged series appear in legend order, top to bottom", {
  term <- data.frame(area = c("cardiovascular", "oncology"), phase = "Phase 3", n_final = 50,
                     n_terminated = c(5, 9), n_withdrawn = 1, stopped_early_rate = c(0.12, 0.20))
  bars <- ggplot2::ggplot_build(plot_termination(term))$data[[1]]
  expect_gt(bars$y[bars$fill == AREA_COLOURS[["oncology"]]],
            bars$y[bars$fill == AREA_COLOURS[["cardiovascular"]]])

  dur <- data.frame(phase = "Phase 3", outcome = c("Terminated", "Completed"), n_trials = 30,
                    p25 = c(5, 20), median = c(10, 30), p75 = c(15, 40))
  points <- ggplot2::ggplot_build(plot_duration(dur))$data[[2]]
  expect_gt(points$y[points$colour == OUTCOME_COLOURS[["Completed"]]],
            points$y[points$colour == OUTCOME_COLOURS[["Terminated"]]])
})

test_that("the sites table view has one column per bucket and fills gaps with 0%", {
  d <- data.frame(phase = c("Phase 1", "Phase 1", "Phase 3"), sites_bucket = c("1", "2-10", ">100"),
                  n_trials = c(8, 2, 5), share_of_phase = c(0.8, 0.2, 1))
  tbl <- table_sites(d)
  expect_equal(names(tbl), c("Phase", "1 site", "2-10 sites", "11-50 sites", "51-100 sites", ">100 sites"))
  expect_equal(tbl$Phase, c("Phase 1", "Phase 3"))
  expect_equal(tbl[["11-50 sites"]], c("0%", "0%"))
})
