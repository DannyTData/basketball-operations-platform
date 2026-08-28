# Posit Connect feedback-candidate entry point.
pkgload::load_all(
  path = ".",
  quiet = TRUE,
  export_all = TRUE,
  helpers = TRUE
)

run_tbi_feedback(
  expires_at = Sys.time() + 8 * 60 * 60
)
