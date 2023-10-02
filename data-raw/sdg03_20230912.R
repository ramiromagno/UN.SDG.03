library(tidyverse)
library(snakecase)

# Source: https://unstats.un.org/sdgs/indicators/database/archive
# Direct URL: https://unstats.un.org/sdgs/indicators/database/archive/2023_Q2.2_AllData_After_20230912_CSV.zip
data_path <- here::here("data-raw/2023_Q2.2_AllData_After_20230912.csv")

spec <-
  cols(
    ID = col_integer(),
    Goal = col_factor(ordered = TRUE),
    Target = col_factor(ordered = TRUE),
    Indicator = col_factor(ordered = TRUE),
    isDSDSeries = col_logical(),
    SeriesID = col_integer(),
    SeriesCode = col_character(),
    SeriesDescription = col_character(),
    SeriesObservationCount = col_integer(),
    GeoAreaCode = col_integer(),
    GeoAreaName = col_factor(),
    TimePeriod = col_factor(levels = as.character(1959:2023), ordered = TRUE),
    BasePeriod = col_factor(levels = as.character(2015:2021), ordered = TRUE),
    Source = col_factor(),
    GeoInfoUrl = col_character(),
    FootNote = col_character(),
    ObservationID = col_double(),
    Age = col_character(),
    Location = col_character(),
    Nature = col_character(),
    Sex = col_character(),
    Units = col_character(),
    `IHR Capacity` = col_factor(),
    `Policy Domains` = col_factor(),
    `Frequency of Chlorophyll-a concentration` = col_factor(levels = c("Moderate", "High", "Extreme"), ordered = TRUE),
    `Report Ordinal` = col_factor(levels = c("FIRST", "SECOND", "THIRD", "FOURTH", "FIFTH", "SIXTH", "SEVENTH", "EIGHTH"), ordered = TRUE),
    `Deviation Level` = col_factor(levels = c("LOW", "MEDIUM", "HIGH", "EXTREME"), ordered = TRUE),
    .default = col_character()
  )

data01 <-
  readr::read_csv(
    data_path,
    col_select = -1L, # Column one is the row number
    col_types = spec,
    locale = locale(encoding = "ISO-8859-1"),
    name_repair = "unique_quiet" # Because column one is not named.
  )

# Drop secondary id columns and `ValueType` (because it simply "follows" the
# type in `Value`)
data02 <- dplyr::select(data01, -c("ID", "ObservationID", "ValueType"))

# Keep only observations pertaining Goal 3
data03 <- dplyr::filter(data02, Goal == 3L)

# Remove non-informative columns, i.e. columns with a single value (all-NAs
# columns also included).
# data04 <- data03[colSums(!is.na(data03)) > 0]
data04 <- Filter(\(x) (length(unique(x)) > 1), data03)

# Convert column names to snake case.
data05 <- data04
colnames(data05) <- snakecase::to_snake_case(colnames(data04))

indicators <-
  dplyr::distinct(
    data05,
    target,
    indicator
  )

data_series <-
  dplyr::distinct(
    data05,
    target,
    indicator,
    series_id,
    series_code,
    series_description,
    series_observation_count
  )

# Keep only the `series_code` column and drop other `series`-columns.
data06 <- dplyr::select(data05, "series_code" | -starts_with("series")) |>
  dplyr::relocate("series_code", .after = "indicator")

# The `value` column contains some values not coercible to double, see below
# the cases. So we deal with them explicitly mapping them to coercible doubles.
# The original values are kept in a column `value_raw` for reference.
data07 <-
  data06 |>
  dplyr::mutate(
    value_raw = value,
    value = case_match(
      value_raw,
      "<0.1" ~ "0.1",
      "<0.5" ~ "0.5",
      "<2.5" ~ "2.5",
      "<5" ~ "5",
      ">95" ~ "95",
      "<100" ~ "100",
      "N" ~ NA_character_,
      .default = value_raw
    ),
    value = as.double(value)
  )

# The data variables are given by each data series, so now I make each series'
# value belong to its own column.
data08 <- dplyr::group_by(data07, target, indicator, series_code)

# Keep key columns but get rid of remaining columns that might have only one value.
key_cols <- c("target", "indicator", "series_code", "geo_area_name", "time_period", "value")
data09 <-
  data08 |>
  dplyr::group_map( ~ dplyr::select(
    .x,
    dplyr::all_of(key_cols) |
      dplyr::where( ~ dplyr::n_distinct(.) > 1)
  ), .keep = TRUE)

series_codes <-
  purrr::map(data09,
             ~ unique(.x$series_code)) |>
  unlist()

data10 <- setNames(data09, series_codes)


# Exported objects
sdg_03_indicators <- indicators
sdg_03_series <- data_series
sdg_03_data <- data10

usethis::use_data(sdg_03_indicators, overwrite = TRUE)
usethis::use_data(sdg_03_series, overwrite = TRUE)
usethis::use_data(sdg_03_data, overwrite = TRUE)


