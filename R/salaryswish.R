get_salaryswish_page <- function(pg = 1) {
  
  url <- paste0(
    "https://www.salaryswish.com/ajax/browse/active?",
    "contract=null&",
    "signing-method=null&",
    "stats-season=2027&",
    "display=weight,height,draft,signing-status,expiry-year,",
    "likely-incentive,unlikely-incentive,caphit-percent,aav,length,",
    "base-salary,type,signing-method,signing-date,extension&",
    "hide=stats&",
    "pg=", pg
  )
  
  raw <- httr2::request(url) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
  
  page_data <- raw$data$results |>
    rvest::read_html() |>
    rvest::html_element("table") |>
    rvest::html_table(fill = TRUE) |>
    janitor::clean_names()
  
  page_data |>
    dplyr::mutate(
      dplyr::across(dplyr::everything(), as.character)
    )
}