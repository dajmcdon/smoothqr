library(dplyr)
library(epidatr)

d <- as.Date("2023-03-10")

x <- pub_covidcast(
  source = "jhu-csse",
  signals = "confirmed_7dav_incidence_prop",
  time_type = "day",
  geo_type = "state",
  time_values = epirange(20201231, 20211231),
  geo_values = "*",
  as_of = d
) |>
  select(geo_value, time_value, case_rate = value)

y <- pub_covidcast(
  source = "jhu-csse",
  signals = "deaths_7dav_incidence_prop",
  time_type = "day",
  geo_type = "state",
  time_values = epirange(20201231, 20211231),
  geo_values = "*",
  as_of = d
) |>
  select(geo_value, time_value, death_rate = value)

covid_case_death_rates <- x |>
  full_join(y, by = c("geo_value", "time_value")) |>
  as_tibble()

usethis::use_data(covid_case_death_rates, overwrite = TRUE)
