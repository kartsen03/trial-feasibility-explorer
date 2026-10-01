# Deploy the app to shinyapps.io. Credentials come from environment variables, set
# from repository secrets in CI; nothing is read from or written to the repo.
#
#   SHINYAPPS_ACCOUNT=... SHINYAPPS_TOKEN=... SHINYAPPS_SECRET=... Rscript scripts/deploy.R

env <- Sys.getenv(c("SHINYAPPS_ACCOUNT", "SHINYAPPS_TOKEN", "SHINYAPPS_SECRET"))
missing <- names(env)[!nzchar(env)]
if (length(missing) > 0) stop("not set: ", paste(missing, collapse = ", "), call. = FALSE)
if (!file.exists("data/trials.sqlite")) stop("data/trials.sqlite is missing", call. = FALSE)

rsconnect::setAccountInfo(
  name = env[["SHINYAPPS_ACCOUNT"]],
  token = env[["SHINYAPPS_TOKEN"]],
  secret = env[["SHINYAPPS_SECRET"]]
)

# Only what the running app needs: no pipeline code, tests or raw data.
rsconnect::deployApp(
  appDir = ".",
  appFiles = c("app.R", "methods.md", "data/trials.sqlite",
               list.files("R", full.names = TRUE), list.files("sql", full.names = TRUE)),
  appName = "trial-feasibility-explorer",
  appTitle = "Clinical Trial Feasibility Explorer",
  account = env[["SHINYAPPS_ACCOUNT"]],
  server = "shinyapps.io",
  forceUpdate = TRUE,
  launch.browser = FALSE
)
