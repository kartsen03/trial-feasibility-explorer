# Download the database snapshot named in data/SNAPSHOT from this repo's GitHub
# releases, instead of rebuilding it from the API (scripts 01 and 02). The SHA-256 is
# checked before the file is put in place.
#
#   Rscript scripts/00_download_snapshot.R

tag <- readLines("data/SNAPSHOT", warn = FALSE)[[1]]
base <- sprintf("https://github.com/kartsen03/trial-feasibility-explorer/releases/download/%s", tag)

fetch <- function(name, path) {
  httr2::request(paste0(base, "/", name)) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform(path = path)
  invisible(path)
}

gz <- tempfile(fileext = ".sqlite.gz")
message("downloading snapshot ", tag)
fetch("trials.sqlite.gz", gz)
expected <- strsplit(readLines(fetch("trials.sqlite.sha256", tempfile()), warn = FALSE)[[1]], "\\s+")[[1]][[1]]

staged <- tempfile(fileext = ".sqlite")
input <- gzfile(gz, "rb")
output <- file(staged, "wb")
repeat {
  chunk <- readBin(input, "raw", 1e7)
  if (length(chunk) == 0) break
  writeBin(chunk, output)
}
close(input)
close(output)

actual <- as.character(openssl::sha256(file(staged)))
if (!identical(actual, expected)) {
  stop("checksum mismatch for ", tag, ": expected ", expected, ", got ", actual, call. = FALSE)
}

dir.create("data", showWarnings = FALSE)
if (!file.copy(staged, "data/trials.sqlite", overwrite = TRUE)) stop("could not write data/trials.sqlite")
message(sprintf("data/trials.sqlite ready (%.1f MB, sha256 verified)", file.size("data/trials.sqlite") / 1e6))
