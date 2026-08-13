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
    now = Sys.time()) {

  expiration <- tbi_demo_expiration()

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


tbi_demo_chrome <- function() {

  shiny::tagList(

    shiny::tags$head(

      shiny::tags$link(
        rel = "stylesheet",
        href = "tbi-assets/tbi_demo.css"
      ),

      shiny::tags$script(
        src = "tbi-assets/tbi_demo.js"
      )
    ),

    shiny::div(
      id = "tbi-demo-status",
      class = "tbi-demo-status",

      shiny::span(
        class = "tbi-demo-lock",
        "PRIVATE DEMO"
      ),

      shiny::span(
        id = "tbi-demo-countdown",
        "Loading access window…"
      )
    ),

    shiny::tags$button(
      id = "tbi-demo-vault-toggle",
      type = "button",
      class = "tbi-demo-vault-toggle",
      "Saved Scenarios"
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
        id = "tbi-demo-scenario-list",
        class = "tbi-demo-scenario-list"
      )
    ),

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
  )
}


tbi_web_demo_ui <- function(request) {

  if (tbi_demo_is_expired()) {
    return(
      tbi_demo_expired_ui()
    )
  }

  base_ui <- app_ui(request)

  shiny::tagList(
    base_ui,
    tbi_demo_chrome()
  )
}


tbi_web_demo_server <- function(
    input,
    output,
    session) {

  if (tbi_demo_is_expired()) {

    session$sendCustomMessage(
      "tbi-demo-expired",
      list(
        expired = TRUE
      )
    )

    return(
      invisible(NULL)
    )
  }

  app_server(
    input,
    output,
    session
  )

  expiration <- tbi_demo_expiration()

  session$sendCustomMessage(
    "tbi-demo-config",
    list(
      demo_id = tbi_demo_id(),
      expires_at = format(
        expiration,
        "%Y-%m-%d %H:%M:%S %Z"
      ),
      remaining_ms = floor(
        tbi_demo_remaining_seconds() * 1000
      )
    )
  )

  shiny::observe({

    shiny::invalidateLater(
      10000,
      session
    )

    if (tbi_demo_is_expired()) {

      session$sendCustomMessage(
        "tbi-demo-expired",
        list(
          expired = TRUE
        )
      )
    }
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

# >>> TBI_UNIQUE_24H_LINKS_START >>>

# ============================================================
# TBI — UNIQUE SIGNED 24-HOUR RECIPIENT LINKS
# ============================================================

tbi_demo_signing_secret <- function() {

  secret <- trimws(
    Sys.getenv(
      'TBI_DEMO_SIGNING_SECRET',
      ''
    )
  )

  if (!nzchar(secret)) {
    stop(
      'TBI_DEMO_SIGNING_SECRET is not configured.',
      call. = FALSE
    )
  }

  secret
}


tbi_demo_token_signature <- function(
    token_id,
    expires_unix) {

  payload <- paste(
    token_id,
    expires_unix,
    sep = '.'
  )

  digest::hmac(
    key = tbi_demo_signing_secret(),
    object = payload,
    algo = 'sha256',
    serialize = FALSE
  )
}


tbi_demo_create_token <- function(
    hours = 24) {

  hours <- suppressWarnings(
    as.numeric(hours)
  )

  if (
    !length(hours) ||
    is.na(hours) ||
    !is.finite(hours) ||
    hours <= 0
  ) {
    hours <- 24
  }

  token_id <- paste0(
    sample(
      c(letters, LETTERS, 0:9),
      32,
      replace = TRUE
    ),
    collapse = ''
  )

  expires_unix <- as.integer(
    Sys.time() + hours * 60 * 60
  )

  signature <- tbi_demo_token_signature(
    token_id,
    expires_unix
  )

  paste(
    token_id,
    expires_unix,
    signature,
    sep = '.'
  )
}


tbi_demo_validate_token <- function(
    token,
    now = Sys.time()) {

  invalid <- list(
    valid = FALSE,
    expired = TRUE,
    expires_at = as.POSIXct(NA),
    remaining_seconds = 0
  )

  if (
    is.null(token) ||
    !length(token) ||
    is.na(token[[1]]) ||
    !nzchar(token[[1]])
  ) {
    return(invalid)
  }

  token <- as.character(token[[1]])

  parts <- strsplit(
    token,
    '.',
    fixed = TRUE
  )[[1]]

  if (length(parts) != 3L) {
    return(invalid)
  }

  token_id <- parts[[1]]
  expires_unix <- suppressWarnings(
    as.numeric(parts[[2]])
  )
  supplied_signature <- parts[[3]]

  if (
    !nzchar(token_id) ||
    !is.finite(expires_unix) ||
    !nzchar(supplied_signature)
  ) {
    return(invalid)
  }

  expected_signature <- tbi_demo_token_signature(
    token_id,
    as.integer(expires_unix)
  )

  signature_ok <- identical(
    tolower(supplied_signature),
    tolower(expected_signature)
  )

  if (!signature_ok) {
    return(invalid)
  }

  expires_at <- as.POSIXct(
    expires_unix,
    origin = '1970-01-01',
    tz = 'UTC'
  )

  remaining <- max(
    0,
    as.numeric(
      difftime(
        expires_at,
        now,
        units = 'secs'
      )
    )
  )

  list(
    valid = remaining > 0,
    expired = remaining <= 0,
    expires_at = expires_at,
    remaining_seconds = remaining,
    token_id = token_id
  )
}


tbi_demo_token_from_query <- function(query_string) {

  if (
    is.null(query_string) ||
    !length(query_string) ||
    is.na(query_string)
  ) {
    return(NULL)
  }

  query <- shiny::parseQueryString(
    query_string
  )

  query[['tbi_access']]
}


tbi_create_24h_link <- function(
    base_url = Sys.getenv('TBI_DEMO_BASE_URL'),
    hours = 24) {

  base_url <- trimws(base_url)

  if (!nzchar(base_url)) {
    stop(
      paste0(
        'Provide the Connect Cloud URL or set ',
        'TBI_DEMO_BASE_URL.'
      ),
      call. = FALSE
    )
  }

  token <- tbi_demo_create_token(
    hours = hours
  )

  separator <- if (
    grepl('?', base_url, fixed = TRUE)
  ) '&' else '?'

  paste0(
    base_url,
    separator,
    'tbi_access=',
    utils::URLencode(
      token,
      reserved = TRUE
    )
  )
}


# ------------------------------------------------------------
# Override web UI with per-link validation
# ------------------------------------------------------------

tbi_web_demo_ui <- function(request) {

  query_string <- ''

  if (
    !is.null(request) &&
    !is.null(request$QUERY_STRING)
  ) {
    query_string <- request$QUERY_STRING
  }

  token <- tbi_demo_token_from_query(
    query_string
  )

  access <- tryCatch(
    tbi_demo_validate_token(token),
    error = function(e) {
      list(
        valid = FALSE,
        expired = TRUE,
        remaining_seconds = 0
      )
    }
  )

  if (!isTRUE(access$valid)) {
    return(
      tbi_demo_expired_ui()
    )
  }

  base_ui <- app_ui(request)

  shiny::tagList(
    base_ui,
    tbi_demo_chrome()
  )
}


# ------------------------------------------------------------
# Override web server with per-link validation
# ------------------------------------------------------------

tbi_web_demo_server <- function(
    input,
    output,
    session) {

  query_string <- session$clientData$url_search

  token <- tbi_demo_token_from_query(
    query_string
  )

  access <- tryCatch(
    tbi_demo_validate_token(token),
    error = function(e) {
      list(
        valid = FALSE,
        expired = TRUE,
        remaining_seconds = 0,
        expires_at = as.POSIXct(NA)
      )
    }
  )

  if (!isTRUE(access$valid)) {

    session$sendCustomMessage(
      'tbi-demo-expired',
      list(
        expired = TRUE
      )
    )

    return(
      invisible(NULL)
    )
  }

  app_server(
    input,
    output,
    session
  )

  session$sendCustomMessage(
    'tbi-demo-config',
    list(
      demo_id = tbi_demo_id(),
      expires_at = format(
        access$expires_at,
        '%Y-%m-%d %H:%M:%S %Z'
      ),
      remaining_ms = floor(
        access$remaining_seconds * 1000
      )
    )
  )

  shiny::observe({

    shiny::invalidateLater(
      10000,
      session
    )

    refreshed <- tryCatch(
      tbi_demo_validate_token(
        token,
        now = Sys.time()
      ),
      error = function(e) {
        list(valid = FALSE)
      }
    )

    if (!isTRUE(refreshed$valid)) {

      session$sendCustomMessage(
        'tbi-demo-expired',
        list(
          expired = TRUE
        )
      )
    }
  })

  invisible(NULL)
}

# <<< TBI_UNIQUE_24H_LINKS_END <<<
