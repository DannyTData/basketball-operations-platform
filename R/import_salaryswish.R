update_salaryswish_data <- function() {
  
  raw <- purrr::map_dfr(
    1:13,
    get_salaryswish_page
  )
  
  clean_salaryswish(raw)
}