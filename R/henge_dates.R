library(tidyverse)
library(suncalc)

# Chelsea NYC
lat = 40.745702
lon = -74.001632

dateset <- getSunlightTimes(date = seq.Date(Sys.Date()-(365*5), Sys.Date()+(365*5), by = 1),
                 keep = c("sunrise", "sunriseEnd", "sunset", "sunsetStart"),
                 lat = lat, lon = lon, tz = "EST")


henge_sunset <- getSunlightPosition(date = dateset$sunsetStart,
                    lat = lat,
                    lon = lon) %>%
  mutate(altitude = altitude * 180 / pi) %>%
  mutate(azimuth = azimuth * 180 / pi) %>%
  mutate(azimuth = ifelse(azimuth >= 0, azimuth + 180, 180 + azimuth)) %>%
  filter(azimuth > 299.8) %>%
  filter(azimuth < 300.2)


henge_sunrise <-   getSunlightPosition(date = dateset$sunriseEnd,
                                     lat = lat,
                                     lon = lon) %>%
  mutate(altitude = altitude * 180 / pi) %>%
  mutate(azimuth = azimuth * 180 / pi) %>%
  mutate(azimuth = ifelse(azimuth >= 0, azimuth + 180, 180 + azimuth)) %>%
  filter(azimuth > 119.8) %>%
  filter(azimuth < 120.2)

# looking for sunset azimuth between 299,8 and 300.2

