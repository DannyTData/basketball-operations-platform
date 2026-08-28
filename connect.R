# Posit Connect feedback-candidate entry point.
pkgload::load_all(
  path = ".",
  quiet = TRUE,
  export_all = TRUE,
  helpers = TRUE
)

run_tbi_feedback(
  expires_at = "2026-08-31 23:59:59",
  timezone = "America/New_York"
)
