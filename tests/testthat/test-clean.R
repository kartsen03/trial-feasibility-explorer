test_that("phases: single, combined in either order, not applicable, and missing", {
  expect_equal(
    normalize_phase(c("PHASE1", "PHASE1|PHASE2", "PHASE2|PHASE1", "PHASE2|PHASE3",
                      "EARLY_PHASE1", "PHASE4", "NA", NA, "")),
    c("Phase 1", "Phase 1/2", "Phase 1/2", "Phase 2/3",
      "Early Phase 1", "Phase 4", "Not applicable", "Not reported", "Not reported")
  )
})

test_that("an unexpected phase combination is labelled Other, not coerced", {
  expect_equal(normalize_phase(c("PHASE1|PHASE3", "PHASE9")), c("Other", "Other"))
})

test_that("month-precision dates are imputed to the 15th and flagged", {
  d <- parse_ct_date(c("2012-08-21", "2012-08", NA, "2012-02-30", "Aug 2012", "2012"))
  expect_equal(d$date, as.Date(c("2012-08-21", "2012-08-15", NA, NA, NA, NA)))
  expect_equal(d$precision, c("day", "month", NA, NA, NA, NA))
})

test_that("duration needs both dates ACTUAL and in order", {
  start <- as.Date(c("2020-01-15", "2020-01-15", "2020-01-15", NA))
  end <- as.Date(c("2021-01-15", "2021-01-15", "2019-01-15", "2021-01-15"))
  d <- duration_months(start, rep("ACTUAL", 4), end, c("ACTUAL", "ESTIMATED", "ACTUAL", "ACTUAL"))
  expect_equal(d[[1]], 366 / (365.25 / 12))  # 2020 is a leap year
  expect_true(all(is.na(d[2:4])))            # estimated end, end before start, no start
  expect_true(is.na(duration_months(as.Date("2020-01-01"), NA, as.Date("2021-01-01"), "ACTUAL")))
})

test_that("common stop reasons map to their categories", {
  expect_equal(
    categorize_stop_reason(c("Low Accrual", "poor recrutment", "Lack of funding",
                             "PI left institution", "Drug supply", "COVID-19 pandemic")),
    c("Low accrual", "Low accrual", "Funding", "Investigator / site", "Drug supply", "COVID-19")
  )
})

test_that("a safety disclaimer doesn't turn a business decision into a safety stop", {
  expect_equal(
    categorize_stop_reason(c(
      "Business decision. No safety concerns contributed to this decision.",
      "Strategic reasons; not related to any safety or efficacy concerns",
      "Terminated for strategic reasons, not due to safety",
      "The trial was stopped due to safety concerns"
    )),
    c(rep("Business / sponsor decision", 3), "Safety")
  )
})

test_that("'no efficacy' is a real reason, while 'futility in recruitment' is accrual", {
  expect_equal(
    categorize_stop_reason(c("No efficacy observed at interim", "Futility in recruitment")),
    c("Efficacy / futility", "Low accrual")
  )
})

test_that("rule order settles text that names several reasons", {
  expect_equal(categorize_stop_reason("Sponsor decision due to slow enrollment"), "Low accrual")
  expect_equal(categorize_stop_reason("PI no longer available"), "Investigator / site")
})

test_that("missing or blank text is uncategorised and unmatched text is Other", {
  expect_equal(categorize_stop_reason(c(NA, "", "   ", "Halted prematurely")),
               c(NA, NA, NA, "Other"))
})

test_that("the categoriser agrees with the hand-labelled validation sample", {
  v <- read.csv(fixture("stop_reasons_validation.csv"), stringsAsFactors = FALSE)
  expect_equal(nrow(v), 100)
  # 84/100 was the blind score when the rules were frozen. This fails on regression.
  expect_gte(mean(categorize_stop_reason(v$why_stopped) == v$label), 0.84)
})

test_that("only stopped trials get a stop category", {
  s <- clean_studies(parse_area_dir(fixture("raw", "oncology"))$studies)
  category <- setNames(s$stop_category, s$nct_id)
  expect_true(is.na(category[["NCT90000001"]]))  # completed
  expect_equal(category[["NCT90000002"]], "Low accrual")
  expect_equal(category[["NCT90000004"]], "Business / sponsor decision")
})

test_that("a stopped trial with no reason text is 'Not reported'", {
  raw <- parse_area_dir(fixture("raw", "oncology"))$studies
  raw$why_stopped[raw$nct_id == "NCT90000002"] <- NA
  s <- clean_studies(raw)
  expect_equal(s$stop_category[s$nct_id == "NCT90000002"], "Not reported")
})

test_that("a missing sponsor class becomes UNKNOWN so the filter can select it", {
  s <- clean_studies(parse_area_dir(fixture("raw", "oncology"))$studies)
  expect_equal(s$sponsor_class[s$nct_id == "NCT90000006"], "UNKNOWN")
})

test_that("month-precision dates still give a duration, recorded as imputed", {
  s <- clean_studies(parse_area_dir(fixture("raw", "oncology"))$studies)
  r <- s[s$nct_id == "NCT90000002", ]
  expect_equal(r$start_date, "2012-08-15")
  expect_equal(r$start_date_precision, "month")
  expect_equal(r$duration_months,
               as.numeric(as.Date("2013-02-15") - as.Date("2012-08-15")) / (365.25 / 12))
})
