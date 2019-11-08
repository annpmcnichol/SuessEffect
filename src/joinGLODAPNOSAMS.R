# Join NOSAMS and glodap data

library(tidyverse)
library(lubridate)
library(here)

glodap <- read_csv(here("data/glodapv2MMF.csv"))
expcodes <- read_delim(here("data/expocodes.txt"), 
                       delim = "\t", 
                       col_names = c("num", "cruise"))
nosams <- read_csv(here("data/nosams_clivar.csv"))

# pick what we need from GLODAP
glodap_dic <- glodap %>%
  mutate(collection_time = ymd_hm(paste(year, month, day, hour, minute, sep = "-"))) %>%
  select(cruise, station, cast, collection_time, 
         latitude, longitude, bottomdepth, maxsampdepth, 
         bottle, pressure, depth, temperature, theta, salinity,
         oxygen, aou, phosphate, tco2, talk,
         c13, c13f, c14, c14f, c14err) %>%
  inner_join(expcodes, by = c("cruise" = "num")) %>%
  rename(expocode = cruise.y)

write_csv(glodap_dic, here("data/glodap_dic.csv"))


# Join the two datasets on cruise, cast, bottle. 
# Using a left join to retain all nosams, but we're missing a 
# fair amount of data from GLODAP. Why?

joined_dic <- left_join(nosams, glodap_dic, by = c("expocode", "station", "cast", "bottle"))

write.csv(joined_dic, here("data/nosams_glodap.csv"))
