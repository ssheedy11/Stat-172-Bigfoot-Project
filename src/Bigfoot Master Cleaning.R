rm(list= ls())

library(mice)
library(tidyverse)
library(dplyr)
library(lubridate)
library(lunar)
library(httr)
library(jsonlite)
library(pbapply)

# Reading in the CSV file
# Note: stringsAsFactors = TRUE makes character columns into Factors. 
# This causes issues with coalesce later, so we will convert specific columns to char when needed.
bigfoot <- read.csv("data/bigfoot.csv")

# 1. FILTERING
# Dropping Class C
bigfoot_clean <- bigfoot %>%
  filter(classification != "Class C") %>%
  select(-c(title, geohash, summary))

# 2. DATE
bigfoot_clean <- bigfoot_clean %>%
  mutate(date = ymd(date))

# 3. OBSERVED
# if observed is missing replace it with location details
bigfoot_clean <- bigfoot_clean %>%
  mutate(observed = if_else(observed == "", location_details, observed)) %>%
  select(-c(location_details)) %>%
  filter(observed != "" & observed != ".")

# 5. LATITUDE AND LONGITUDE
# FIX: We use bigfoot1 here (not bigfoot) to preserve previous filtering
lat_long <- c("latitude", "longitude")

# Global medians
global_medians_geo <- bigfoot_clean %>%
  summarise(across(all_of(lat_long), ~ median(.x, na.rm = TRUE)))

# State medians
group_medians_geo <- bigfoot_clean %>%
  group_by(state) %>%
  summarise(
    median_latitude  = median(latitude,  na.rm = TRUE),
    median_longitude = median(longitude, na.rm = TRUE),
    .groups = "drop"
  )

bigfoot_clean <- bigfoot_clean %>%
  left_join(group_medians_geo, by = "state") %>%
  mutate(
    latitude  = if_else(is.na(latitude),  median_latitude,  latitude),
    longitude = if_else(is.na(longitude), median_longitude, longitude),
    # If still NA (state had no data), use global median
    latitude  = if_else(is.na(latitude),  global_medians_geo$latitude,  latitude),
    longitude = if_else(is.na(longitude), global_medians_geo$longitude, longitude)
  ) %>%
  select(-median_latitude, -median_longitude)

# 6. MOON PHASE
moon_phase_fraction <- function(date) {
  ref_new_moon <- as.Date("2000-01-06")
  lunar_month <- 29.53058867
  days_since_new <- as.numeric(date - ref_new_moon)
  phase <- (days_since_new %% lunar_month) / lunar_month
  return(phase)
}

bigfoot_clean <- bigfoot_clean %>%
  mutate(
    moon_phase = ifelse(
      moon_phase < 0 | moon_phase > 1 | is.na(moon_phase),
      moon_phase_fraction(date),
      moon_phase
    )
  )

moon_median <- median(bigfoot_clean$moon_phase, na.rm = TRUE)
bigfoot_clean <- bigfoot_clean %>%
  mutate(moon_phase = if_else(is.na(moon_phase), moon_median, moon_phase))

# 7. PRECIP TYPE (The API Fix)
# Fix: Ensure precip_type is character, not Factor, before we start
bigfoot_clean <- bigfoot_clean %>% 
  mutate(precip_type = as.character(precip_type))

# Logic: If Type is blank but Intensity is 0, it is "none"
bigfoot_clean <- bigfoot_clean %>% 
  mutate(precip_type = case_when(
    precip_type == "" & precip_intensity == 0 ~ "none",
    TRUE ~ precip_type
  ))

get_daily_weather_type <- function(lat, lon, date) {
  url <- paste0(
    "https://archive-api.open-meteo.com/v1/archive?",
    "latitude=", lat,
    "&longitude=", lon,
    "&start_date=", date,
    "&end_date=", date,
    "&daily=snowfall_sum,rain_sum",
    "&timezone=UTC"
  )
  
  # Wrap in tryCatch to prevent script crashing on API fail
  tryCatch({
    res <- fromJSON(content(GET(url), "text", encoding = "UTF-8"))
    if (is.null(res$daily) || length(res$daily$snowfall_sum) == 0) return(NA_character_)
    
    snow <- res$daily$snowfall_sum
    rain <- res$daily$rain_sum
    
    if (snow > 0) return("snow")
    if (rain > 0) return("rain")
    return("none")
  }, error = function(e) return(NA_character_))
}

bigfoot_clean <- bigfoot_clean %>% mutate(row_id = row_number())

# Subset rows needed for API
precip_nulls <- bigfoot_clean %>%
  filter(is.na(precip_type) | precip_type == "") %>%
  filter(!is.na(latitude), !is.na(longitude), !is.na(date))

# Query API
precip_nulls$precip_type_api <- pbsapply(seq_len(nrow(precip_nulls)), function(i) {
  get_daily_weather_type(
    lat = precip_nulls$latitude[i],
    lon = precip_nulls$longitude[i],
    date = precip_nulls$date[i]
  )
})

# Merge API values back
bigfoot_clean <- bigfoot_clean %>%
  left_join(
    precip_nulls %>% select(row_id, precip_type_api),
    by = "row_id"
  ) %>%
  mutate(
    # FIX: Ensure the API column is strictly character (flattening the list)
    precip_type_api = unlist(lapply(precip_type_api, function(x) if(is.null(x)) NA_character_ else as.character(x))),
    precip_type = if_else(
      is.na(precip_type) | precip_type == "",
      coalesce(precip_type_api, "none"),
      precip_type
    )
  ) %>%
  select(-row_id, -precip_type_api)

bigfoot_clean <- bigfoot_clean %>%
  mutate(precip_type = as.factor(precip_type))

# 8. REGIONS
northeast_states <- c("Connecticut", "Maine", "Massachusetts", "New Hampshire", 
                      "Rhode Island", "Vermont", "New Jersey", "New York", "Pennsylvania")
midwest_states <- c("Illinois", "Indiana", "Michigan", "Ohio", "Wisconsin", 
                    "Iowa", "Kansas", "Minnesota", "Missouri", "Nebraska", 
                    "North Dakota", "South Dakota")
south_states <- c("Delaware", "Florida", "Georgia", "Maryland", "North Carolina", 
                  "South Carolina", "Virginia", "West Virginia", "Alabama", 
                  "Kentucky", "Mississippi", "Tennessee", "Arkansas", 
                  "Louisiana", "Oklahoma", "Texas")
west_states <- c("Arizona", "Colorado", "Idaho", "Montana", "Nevada", 
                 "New Mexico", "Utah", "Wyoming", "Alaska", "California", 
                 "Hawaii", "Oregon", "Washington")

bigfoot_clean <- bigfoot_clean %>%
  mutate(region = case_when(
    state %in% northeast_states ~ "Northeast",
    state %in% midwest_states   ~ "Midwest",
    state %in% south_states     ~ "South",
    state %in% west_states      ~ "West",
    TRUE                        ~ "Other/Unknown"
  ))

# Convert to factors
bigfoot_clean <- bigfoot_clean %>%
  mutate(
    region = as.factor(region)
  )

# 9. WEATHER IMPUTATION
weather_cols <- c("temperature_high", "temperature_mid", "temperature_low", 
                  "pressure", "humidity", "precip_probability", 
                  "precip_intensity", "cloud_cover", "dew_point", "wind_speed", "wind_bearing", "uv_index", "visibility")

global_medians_weather <- bigfoot_clean %>%
  summarise(across(all_of(weather_cols), ~ median(.x, na.rm = TRUE))) %>%
  as.list()

group_medians_weather <- bigfoot_clean %>%
  group_by(region, season) %>%
  summarise(
    across(all_of(weather_cols), ~ median(.x, na.rm = TRUE), .names = "median_{.col}"),
    .groups = 'drop'
  )

bigfoot_clean <- bigfoot_clean %>%
  left_join(group_medians_weather, by = c("region", "season"))

for (col in weather_cols) {
  median_col_name <- paste0("median_", col)
  global_median_val <- global_medians_weather[[col]]
  
  bigfoot_clean <- bigfoot_clean %>%
    mutate(
      !!col := coalesce(!!sym(col), !!sym(median_col_name), global_median_val)
    )
}

bigfoot_clean <- bigfoot_clean %>%
  select(-starts_with("median_"))

# 10. DAY OF WEEK
bigfoot_clean$day <- wday(
  bigfoot_clean$date,
  label = FALSE,
  week_start = getOption("lubridate.week.start", 7),
  locale = Sys.getlocale("LC_TIME")
)

# Handle Unknown days (NAs from missing dates)
bigfoot_clean <- bigfoot_clean %>% 
  mutate(day = ifelse(day %in% 1:7, day, "Unknown"))

bigfoot_clean$day <- factor(bigfoot_clean$day,
                            levels = c(1, 2, 3, 4, 5, 6, 7, "Unknown"),
                            labels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday", "Unknown"))

# 11. MONTH
bigfoot_clean <- bigfoot_clean %>%
  mutate(month = ifelse(!is.na(date),month.name[month(date)], "Unknown")) %>%
  select(-c(season,date))

#12. DROPPING STATE AND COUNTY
bigfoot_clean <- bigfoot_clean %>% select(-c(county,state))

# Final Check
str(bigfoot_clean)
summary(bigfoot_clean)
