# ============================================================
# Thompson's Basketball Intelligence
# Secure Web Demo Layer — Phase 15J
# ============================================================


tbi_demo_mode <- function() {
  tolower(
    trimws(
      Sys.getenv(
        "TBI_DEMO_MODE",
        "false"
      )
    )
  ) %in% c(
    "1",
    "true",
    "yes",
    "on"
  )
}


tbi_demo_expiration_disabled <- function() {
  tolower(
    trimws(
      Sys.getenv(
        "TBI_DISABLE_DEMO_EXPIRATION",
        "false"
      )
    )
  ) %in% c(
    "1",
    "true",
    "yes",
    "on"
  )
}


tbi_demo_timezone <- function() {
  value <- trimws(
    Sys.getenv(
      "TBI_DEMO_TIMEZONE",
      "America/New_York"
    )
  )

  if (!nzchar(value)) {
    value <- "America/New_York"
  }

  value
}


tbi_demo_expiration <- function() {

  raw <- trimws(
    Sys.getenv(
      "TBI_DEMO_EXPIRES_AT",
      ""
    )
  )

  if (!nzchar(raw)) {
    return(
      as.POSIXct(
        NA
      )
    )
  }

  tz <- tbi_demo_timezone()

  parsed <- suppressWarnings(
    as.POSIXct(
      raw,
      format = "%Y-%m-%d %H:%M:%S",
      tz = tz
    )
  )

  parsed
}


tbi_demo_is_expired <- function(
    now = Sys.time()) {

  if (!tbi_demo_mode()) {
    return(FALSE)
  }

  if (tbi_demo_expiration_disabled()) {
    return(FALSE)
  }

  expiration <- tbi_demo_expiration()

  if (
    !length(expiration) ||
    is.na(expiration)
  ) {
    return(TRUE)
  }

  now >= expiration
}


tbi_demo_remaining_seconds <- function(
    now = Sys.time(),
    expiration = tbi_demo_expiration()) {

  if (
    !length(expiration) ||
    is.na(expiration)
  ) {
    return(0)
  }

  max(
    0,
    as.numeric(
      difftime(
        expiration,
        now,
        units = "secs"
      )
    )
  )
}


tbi_demo_expiration_delay_ms <- function(
    now = Sys.time(),
    expiration = tbi_demo_expiration(),
    max_interval_ms = 10000L) {
  max_interval_ms <- suppressWarnings(as.numeric(max_interval_ms[[1L]]))
  if (!is.finite(max_interval_ms) || max_interval_ms < 1) {
    stop("max_interval_ms must be a positive finite number.", call. = FALSE)
  }

  remaining_ms <- ceiling(
    tbi_demo_remaining_seconds(
      now = now,
      expiration = expiration
    ) * 1000
  )

  as.integer(max(1, min(max_interval_ms, remaining_ms)))
}


tbi_demo_session_config <- function(
    expiration_disabled = tbi_demo_expiration_disabled(),
    expiration = tbi_demo_expiration(),
    demo_id = tbi_demo_id(),
    now = Sys.time()) {

  expiration_disabled <- isTRUE(expiration_disabled)

  list(
    demo_id = demo_id,
    expires_at = if (expiration_disabled) NULL else format(
      expiration,
      "%Y-%m-%d %H:%M:%S %Z"
    ),
    remaining_ms = if (expiration_disabled) NULL else floor(
      tbi_demo_remaining_seconds(now = now, expiration = expiration) * 1000
    ),
    expiration_disabled = expiration_disabled
  )
}


tbi_demo_expiration_observer_enabled <- function(
    expiration_disabled = tbi_demo_expiration_disabled()) {

  !isTRUE(expiration_disabled)
}


tbi_demo_id <- function() {

  value <- trimws(
    Sys.getenv(
      "TBI_DEMO_ID",
      "tbi-private-demo"
    )
  )

  if (!nzchar(value)) {
    value <- "tbi-private-demo"
  }

  value
}


tbi_feedback_config <- function(expires_at,
                                source_db = NULL,
                                timezone = "America/New_York",
                                now = Sys.time()) {
  timezone <- trimws(as.character(timezone %||% ""))
  if (length(timezone) != 1L || is.na(timezone) || !nzchar(timezone)) {
    stop("A valid feedback timezone is required.", call. = FALSE)
  }
  if (!timezone %in% OlsonNames()) {
    stop("A valid feedback timezone is required.", call. = FALSE)
  }

  expiration <- if (inherits(expires_at, "POSIXt")) {
    as.POSIXct(expires_at, tz = timezone)
  } else {
    value <- as.character(expires_at %||% "")
    if (length(value) != 1L || is.na(value) || !nzchar(trimws(value))) {
      as.POSIXct(NA)
    } else {
      suppressWarnings(as.POSIXct(
        trimws(value),
        format = "%Y-%m-%d %H:%M:%S",
        tz = timezone
      ))
    }
  }
  if (length(expiration) != 1L || is.na(expiration)) {
    stop(
      "Feedback expiration must be explicit and use YYYY-mm-dd HH:MM:SS or POSIXt form.",
      call. = FALSE
    )
  }
  if (expiration <= now) {
    stop("Feedback expiration must be in the future.", call. = FALSE)
  }

  structure(list(
    mode = "feedback",
    expiration = expiration,
    expiration_disabled = FALSE,
    source_db = tbi_resolve_feedback_source_db(source_db),
    timezone = timezone,
    demo_id = "tbi-feedback",
    status_label = "FEEDBACK / NON-AUTHORITATIVE",
    tpe_test_mode = FALSE
  ), class = "tbi_feedback_config")
}


tbi_feedback_is_expired <- function(feedback_config, now = Sys.time()) {
  if (!inherits(feedback_config, "tbi_feedback_config")) {
    stop("feedback_config must be created by tbi_feedback_config().", call. = FALSE)
  }
  expiration <- feedback_config$expiration
  length(expiration) != 1L || is.na(expiration) || now >= expiration
}


tbi_feedback_scenario_vault_policy <- function(
    scenario = NULL,
    feedback_mode = TRUE) {
  feedback_mode <- isTRUE(feedback_mode)
  trade_local <- feedback_mode && tbi_scenario_is_trade_local(scenario)
  unsupported_message <- paste(
    "Multi-team scenario save/restore is not supported in V2 feedback."
  )

  list(
    supported = !trade_local,
    code = if (trade_local) {
      "TRADE_LOCAL_VAULT_UNSUPPORTED"
    } else {
      "SCENARIO_VAULT_SUPPORTED"
    },
    scenario_scope = tbi_scenario_scope_value(scenario) %||% "BASELINE",
    scenario_type = if (is.list(scenario) && isTRUE(scenario$active)) {
      as.character(scenario$scenario_type %||% "")[[1]]
    } else {
      "baseline"
    },
    message = if (trade_local) unsupported_message else NULL
  )
}


tbi_feedback_owned_root <- function(root) {
  if (is.null(root) || length(root) != 1L || is.na(root) || !dir.exists(root)) {
    return(FALSE)
  }
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  temp_root <- normalizePath(tempdir(), winslash = "/", mustWork = TRUE)
  startsWith(tolower(root), paste0(tolower(temp_root), "/")) &&
    startsWith(basename(root), "tbi-feedback-session-")
}


tbi_cleanup_feedback_database <- function(root, unlink_fn = unlink) {
  if (is.null(root) || !length(root) || !dir.exists(root)) {
    return(invisible(TRUE))
  }
  if (!is.function(unlink_fn)) {
    stop("unlink_fn must be a function.", call. = FALSE)
  }
  if (!tbi_feedback_owned_root(root)) {
    stop("Refusing to clean a feedback database outside its owned temp root.", call. = FALSE)
  }
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  database <- file.path(root, "tbi.sqlite")
  sidecars <- c(
    paste0(database, "-wal"),
    paste0(database, "-shm"),
    paste0(database, "-journal"),
    paste0(database, ".journal")
  )
  existing_sidecars <- sidecars[file.exists(sidecars)]

  if (length(existing_sidecars)) {
    try(
      unlink_fn(existing_sidecars, recursive = FALSE, force = TRUE),
      silent = TRUE
    )
  }
  try(
    unlink_fn(root, recursive = TRUE, force = TRUE),
    silent = TRUE
  )

  removed <- !dir.exists(root) && !any(file.exists(c(database, sidecars)))
  if (!removed) {
    warning(
      paste(
        "Failed to completely remove disposable feedback database:",
        root
      ),
      call. = FALSE
    )
  }

  invisible(removed)
}


tbi_prepare_feedback_session <- function(session, source_db) {
  source_db <- tbi_resolve_feedback_source_db(source_db)
  root <- tempfile(pattern = "tbi-feedback-session-", tmpdir = tempdir())
  if (!dir.create(root, recursive = FALSE, showWarnings = FALSE)) {
    stop("Could not create the feedback session database directory.", call. = FALSE)
  }
  prepared <- FALSE
  on.exit({
    if (!prepared && dir.exists(root)) tbi_cleanup_feedback_database(root)
  }, add = TRUE)

  database <- file.path(root, "tbi.sqlite")
  copied <- file.copy(source_db, database, overwrite = FALSE, copy.mode = TRUE)
  if (!isTRUE(copied) || !file.exists(database)) {
    stop("Could not create the disposable feedback session database.", call. = FALSE)
  }
  if (!identical(unname(tools::md5sum(source_db)), unname(tools::md5sum(database)))) {
    stop("The disposable feedback database did not match its source copy.", call. = FALSE)
  }

  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  database <- normalizePath(database, winslash = "/", mustWork = TRUE)
  if (!tbi_feedback_owned_root(root)) {
    stop("The feedback database root failed ownership validation.", call. = FALSE)
  }
  if (is.null(session$userData)) {
    stop("The Shiny session cannot hold feedback isolation state.", call. = FALSE)
  }

  session$userData$tbi_feedback_mode <- TRUE
  session$userData$tbi_feedback_db_root <- root
  session$userData$tbi_feedback_db_path <- database
  session$userData$tbi_feedback_source_db <- source_db
  session$userData$tbi_feedback_tpe_test_mode <- FALSE
  session$onSessionEnded(function() {
    session$userData$tbi_feedback_db_path <- NULL
    session$userData$tbi_feedback_db_root <- NULL
    session$userData$tbi_feedback_cleanup_succeeded <- isTRUE(
      tbi_cleanup_feedback_database(root)
    )
  })
  prepared <- TRUE
  database
}


tbi_demo_terminate_expired_session <- function(session) {
  if (!is.null(session$userData)) {
    session$userData$tbi_demo_expired <- TRUE
  }
  signal_result <- try(
    session$sendCustomMessage(
      "tbi-demo-expired",
      list(expired = TRUE)
    ),
    silent = TRUE
  )
  if (!is.null(session$userData)) {
    session$userData$tbi_demo_expiration_signal_sent <-
      !inherits(signal_result, "try-error")
  }
  session$close()
  invisible(TRUE)
}


tbi_demo_expired_ui <- function() {

  shiny::fluidPage(
    shiny::tags$head(
      shiny::tags$meta(
        name = "viewport",
        content = "width=device-width, initial-scale=1"
      ),
      shiny::tags$style(
        shiny::HTML(
          paste0(
            "html,body{margin:0;background:#07111f;color:#f8fafc;",
            "font-family:Inter,Arial,sans-serif;}",
            ".tbi-expired-wrap{min-height:100vh;display:flex;",
            "align-items:center;justify-content:center;padding:30px;",
            "box-sizing:border-box;background:radial-gradient(circle at top,",
            "#13243b 0,#07111f 55%,#040913 100%);}",
            ".tbi-expired-card{width:min(640px,100%);padding:42px;",
            "border:1px solid rgba(148,163,184,.18);border-radius:18px;",
            "background:rgba(10,20,34,.94);box-shadow:0 28px 80px rgba(0,0,0,.38);",
            "text-align:center;}",
            ".tbi-expired-logo{width:58px;height:58px;margin:0 auto 20px;",
            "display:flex;align-items:center;justify-content:center;border-radius:14px;",
            "background:#246bfd;font-weight:900;font-size:20px;}",
            ".tbi-expired-card h1{margin:0 0 12px;font-size:30px;}",
            ".tbi-expired-card p{margin:0;color:#aab8c9;font-size:16px;line-height:1.65;}"
          )
        )
      )
    ),

    shiny::div(
      class = "tbi-expired-wrap",

      shiny::div(
        class = "tbi-expired-card",

        shiny::div(
          class = "tbi-expired-logo",
          "TBI"
        ),

        shiny::h1(
          "Private Demo Access Expired"
        ),

        shiny::p(
          paste(
            "This temporary demonstration of Thompson's Basketball",
            "Intelligence is no longer active."
          )
        )
      )
    )
  )
}


tbi_demo_chrome <- function(
    expiration_disabled = tbi_demo_expiration_disabled(),
    status_label = "PRIVATE DEMO",
    feedback_mode = FALSE) {

  expiration_disabled <- isTRUE(expiration_disabled)
  www_path <- app_sys("app/www")

  if (!nzchar(www_path) || !dir.exists(www_path)) {
    www_path <- file.path("inst", "app", "www")
  }

  demo_script_path <- file.path(www_path, "tbi_demo.js")
  demo_script_src <- paste0(
    "tbi-assets/tbi_demo.js?v=",
    unname(tools::md5sum(demo_script_path))
  )

  shiny::tagList(

    shiny::tags$head(

      shiny::tags$script(
        shiny::HTML(
          paste0(
            sprintf(
              "window.TBI_DEMO_EXPIRATION_DISABLED = %s;",
              if (expiration_disabled) "true" else "false"
            ),
            sprintf(
              "window.TBI_FEEDBACK_MODE = %s;",
              if (isTRUE(feedback_mode)) "true" else "false"
            ),
            "document.documentElement.classList.add('tbi-demo-mode');",
            if (isTRUE(feedback_mode)) {
              "document.documentElement.classList.add('tbi-feedback-mode');"
            } else {
              ""
            }
          )
        )
      ),

      shiny::tags$link(
        rel = "stylesheet",
        href = "tbi-assets/tbi_demo.css"
      ),

      shiny::tags$script(
        src = demo_script_src
      )
    ),

    shiny::div(
      class = "tbi-demo-dock",

      shiny::div(
        id = "tbi-demo-status",
        class = "tbi-demo-status",

        shiny::span(
          class = "tbi-demo-lock",
          status_label
        ),

        if (isTRUE(feedback_mode)) {
          shiny::span(
            class = "tbi-demo-status-context",
            "Scenario exploration only"
          )
        },

        shiny::span(
          id = "tbi-demo-countdown",
          if (expiration_disabled) {
            "Expiration disabled for local development"
          } else {
            "Loading access window…"
          }
        )
      ),

      shiny::tags$button(
        id = "tbi-demo-vault-toggle",
        type = "button",
        class = "tbi-demo-vault-toggle",
        `aria-label` = "Open saved scenarios",
        "Scenarios"
      )
    ),

    shiny::div(
      id = "tbi-demo-vault",
      class = "tbi-demo-vault",

      shiny::div(
        class = "tbi-demo-vault-header",

        shiny::div(
          shiny::strong(
            "Temporary Scenario Vault"
          ),

          shiny::span(
            "Stored only in this browser during the demo window."
          )
        ),

        shiny::tags$button(
          id = "tbi-demo-vault-close",
          type = "button",
          class = "tbi-demo-icon-button",
          "×"
        )
      ),

      shiny::div(
        class = "tbi-demo-vault-actions",

        shiny::tags$button(
          id = "tbi-demo-save",
          type = "button",
          class = "tbi-demo-primary-button",
          "Save Current Scenario"
        ),

        shiny::tags$button(
          id = "tbi-demo-clear",
          type = "button",
          class = "tbi-demo-secondary-button",
          "Clear Saved"
        )
      ),

      shiny::div(
        id = "tbi-demo-vault-notice",
        class = "tbi-demo-empty",
        `aria-live` = "polite",
        hidden = "hidden"
      ),

      shiny::div(
        id = "tbi-demo-scenario-list",
        class = "tbi-demo-scenario-list"
      )
    ),

    if (!expiration_disabled) {
      shiny::div(
        id = "tbi-demo-expiration-overlay",
        class = "tbi-demo-expiration-overlay",

        shiny::div(
          class = "tbi-demo-expiration-card",

          shiny::div(
            class = "tbi-demo-expiration-logo",
            "TBI"
          ),

          shiny::h2(
            "Demo Access Expired"
          ),

          shiny::p(
            "This temporary TBI workspace is no longer active."
          )
        )
      )
    }
  )
}


tbi_web_demo_ui <- function(request, feedback_config = NULL) {

  feedback_mode <- inherits(feedback_config, "tbi_feedback_config")
  expired <- if (feedback_mode) {
    tbi_feedback_is_expired(feedback_config)
  } else {
    tbi_demo_is_expired()
  }
  if (expired) {
    return(
      tbi_demo_expired_ui()
    )
  }

  base_ui <- app_ui(request)

  shiny::tagList(
    base_ui,
    tbi_demo_chrome(
      expiration_disabled = if (feedback_mode) FALSE else tbi_demo_expiration_disabled(),
      status_label = if (feedback_mode) feedback_config$status_label else "PRIVATE DEMO",
      feedback_mode = feedback_mode
    )
  )
}


tbi_web_demo_server <- function(
    input,
    output,
    session,
    feedback_config = NULL,
    time_now = Sys.time) {

  if (!is.function(time_now)) {
    stop("time_now must be a function.", call. = FALSE)
  }

  feedback_mode <- inherits(feedback_config, "tbi_feedback_config")
  is_expired <- function(now) {
    if (feedback_mode) {
      tbi_feedback_is_expired(feedback_config, now = now)
    } else {
      tbi_demo_is_expired(now = now)
    }
  }
  initial_now <- shiny::isolate(time_now())

  if (is_expired(initial_now)) {
    tbi_demo_terminate_expired_session(session)
    return(invisible(NULL))
  }

  if (feedback_mode) {
    Sys.setenv(TBI_FEEDBACK_MODE = "true")
    tbi_prepare_feedback_session(session, feedback_config$source_db)
  }

  app_server(
    input,
    output,
    session
  )

  expiration_disabled <- if (feedback_mode) FALSE else tbi_demo_expiration_disabled()
  session_demo_id <- if (feedback_mode) {
    token <- as.character(session$token %||% basename(session$userData$tbi_feedback_db_root))
    paste(feedback_config$demo_id, token[[1]], sep = "-")
  } else {
    tbi_demo_id()
  }
  config <- tbi_demo_session_config(
    expiration_disabled = expiration_disabled,
    expiration = if (feedback_mode) feedback_config$expiration else tbi_demo_expiration(),
    demo_id = session_demo_id,
    now = initial_now
  )
  config$feedback_mode <- feedback_mode

  session$sendCustomMessage(
    "tbi-demo-config",
    config
  )

  if (!tbi_demo_expiration_observer_enabled(expiration_disabled)) {
    return(invisible(NULL))
  }

  shiny::observe({
    now <- time_now()
    if (is_expired(now)) {
      tbi_demo_terminate_expired_session(session)
      return(invisible(NULL))
    }

    expiration <- if (feedback_mode) {
      feedback_config$expiration
    } else {
      tbi_demo_expiration()
    }

    shiny::invalidateLater(
      tbi_demo_expiration_delay_ms(now = now, expiration = expiration),
      session
    )
  })

  invisible(NULL)
}


tbi_demo_local_expiration <- function(
    hours = 24) {

  hours <- suppressWarnings(
    as.numeric(hours)
  )

  if (
    !length(hours) ||
    is.na(hours) ||
    hours <= 0
  ) {
    hours <- 24
  }

  format(
    Sys.time() + hours * 60 * 60,
    "%Y-%m-%d %H:%M:%S",
    tz = tbi_demo_timezone()
  )
}
