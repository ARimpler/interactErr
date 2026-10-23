library(golem)

devtools::build()

golem::run_dev()
devtools::document()
devtools::load_all()

devtools::build()


devtools::check(
  pkg = ".",
  cran = TRUE,
  remote = FALSE,
  incoming = TRUE,
  manual = FALSE,
  force_suggests = FALSE,
  run_dont_test = FALSE,
  args = "--timings",
  env_vars = c(NOT_CRAN = "true"),
  quiet = FALSE,
  vignettes = TRUE,
  error_on = "warning"
)


usethis::use_test("functions")

tools::showNonASCIIfile("R/text.R")

usethis::use_citation()

devtools::test()

usethis::use_gpl_license(
  version = 3,
  include_future = TRUE
)

usethis::use_github_action("check-standard")
