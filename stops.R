library(tidyverse)
library(jsonlite)
library(lubridate)
library(lutz)
library(suncalc)

# Utility
time_to_minute <- function(time) {
  hour(hms(time)) * 60 + minute(hms(time))
}

# SF timezone
center_lat <- 37.7749300
center_lng <- -122.4194200
tz <- lutz::tz_lookup_coords(center_lat, center_lng, warn = T)

# Data
# Downloaded from https://openpolicing.stanford.edu/data/
data <-  read_rds("raw/yg821jf8611_ca_san_francisco_2020_04_01.rds") %>% mutate(year = year(date))
y2015 <- data %>% filter(year == 2015) %>% mutate(date = ymd(str_sub(date, 1, 10)))

sunset_times <- y2015 %>%
  # use center lat & long of San Francisco
  mutate(lat=center_lat, lon = center_lng) %>%
  select(date, lat, lon) %>%
  distinct() %>%
  getSunlightTimes(
    data = .,
    keep = c("sunset", "dusk"),
    tz = tz
  ) %>% 
  mutate_at(vars("sunset", "dusk"), ~format(., "%H:%M:%S")) %>% 
  mutate(
    sunset_minute = time_to_minute(sunset),
    dusk_minute = time_to_minute(dusk)
  ) %>% select(date, sunset, dusk, ends_with("minute"))

vod_stops <- 
  y2015 %>% 
  left_join(
    sunset_times,
    by ="date"
  ) %>% 
  mutate(
    minute = time_to_minute(time),
    minutes_after_dark = minute - dusk_minute,
    is_dark = minute > dusk_minute,
    min_dusk_minute = min(dusk_minute),
    max_dusk_minute = max(dusk_minute),
    is_black = subject_race == "black"
  ) %>% 
  filter(
    # Filter to get only the inter-twilight period
    minute >= min_dusk_minute,
    minute <= max_dusk_minute,
    # Remove ambiguous period between sunset and dusk
    !(minute > sunset_minute & minute < dusk_minute),
    # Compare only white and black drivers
    subject_race %in% c("black", "white")
  )

# Export file
forExport <- vod_stops %>% 
  mutate(period = ifelse(is_dark==FALSE, "day", "night"), 
         subject_race = ifelse(is_black==TRUE, "black", "white")) %>%
  select(time, minute, minutes_after_dark, period, subject_race)

write.csv(forExport, "tidy/stops-sf-2015.csv")

# veil-of-darkness chart with ggplot
window <- vod_stops %>% 
  select(time, minutes_after_dark, is_dark, is_black) %>%
  mutate(time = hms(time)) %>%  
  filter(time > hm("18:00"), time < hm("18:15"))
 
chartData <- window %>% 
  mutate(bin = cut(minutes_after_dark, breaks = c(-90, -80, -70,-60, -50, -40, 0, 10, 20, 30, 50, 60))) %>% 
  group_by(bin, is_dark) %>% 
  summarize(prop_black = mean(is_black), nbStops = n()) %>%
  drop_na()

veil_plot <- ggplot(
  data = chartData,
  mapping = aes(x = bin, y = prop_black)) + 
  geom_point(
    mapping = aes(color = is_dark, size = nbStops)
  ) + 
  geom_smooth(method = "lm") +
  theme_minimal()

# Regression models
mod1 <- glm(
  is_black ~ is_dark + splines::ns(minute, df = 6),
  family = binomial,
  data = vod_stops
)
summary(mod1)$coefficients["is_darkTRUE", c("Estimate", "Std. Error")]

mod2 <- glm(
  is_black ~ is_dark + splines::ns(minute, df = 6) + as.factor(district),
  family = binomial,
  data = vod_stops
)

summary(mod2)$coefficients["is_darkTRUE", c("Estimate", "Std. Error")]


