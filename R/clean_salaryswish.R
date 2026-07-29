clean_salaryswish <- function(df) {
  
  df |>
    dplyr::mutate(
      player = sub("^\\d+\\.\\s*", "", player),
      
      age = readr::parse_number(age),
      weight = readr::parse_number(weight),
      
      contract_length = readr::parse_number(length),
      
      cap_hit = readr::parse_number(cap_hit),
      cap_hit_percent = readr::parse_number(cap_hit_percent),
      
      aav = readr::parse_number(aav),
      base_salary = readr::parse_number(base_salary),
      
      likely_incentive =
        readr::parse_number(likely_incentive),
      
      unlikely_incentive =
        readr::parse_number(unlikely_incentive),
      
      signing_status = signing,
      expiry_status = expiry,
      
      is_extension = extension == "✔"
    )
}