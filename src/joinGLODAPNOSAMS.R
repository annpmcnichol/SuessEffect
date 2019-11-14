# Join NOSAMS and glodap data

library(tidyverse)
library(lubridate)
library(here)


colspec <- cols(
  cruise = col_integer(),
  station = col_integer(),
  cast = col_integer(),
  year = col_integer(),
  month = col_integer(),
  day = col_integer(),
  hour = col_integer(),
  minute = col_integer(),
  latitude = col_double(),
  longitude = col_double(),
  bottomdepth = col_double(),
  maxsampdepth = col_double(),
  bottle = col_integer(),
  pressure = col_double(),
  depth = col_double(),
  temperature = col_double(),
  theta = col_double(),
  salinity = col_double(),
  salinityf = col_integer(),
  salinityqc = col_double(),
  sigma0 = col_double(),
  sigma1 = col_double(),
  sigma2 = col_double(),
  sigma3 = col_double(),
  sigma4 = col_double(),
  gamma = col_double(),
  oxygen = col_double(),
  oxygenf = col_integer(),
  oxygenqc = col_double(),
  aou = col_double(),
  aouf = col_integer(),
  nitrate = col_double(),
  nitratef = col_integer(),
  nitrateqc = col_double(),
  nitrite = col_double(),
  nitritef = col_double(),
  silicate = col_double(),
  silicatef = col_double(),
  silicateqc = col_double(),
  phosphate = col_double(),
  phosphatef = col_double(),
  phosphateqc = col_double(),
  tco2 = col_double(),
  tco2f = col_double(),
  tco2qc = col_double(),
  talk = col_double(),
  talkf = col_double(),
  talkqc = col_double(),
  phts25p0 = col_double(),
  phts25p0f = col_double(),
  phtsinsitutp = col_double(),
  phtsinsitutpf = col_double(),
  phtsqc = col_double(),
  cfc11 = col_double(),
  pcfc11 = col_double(),
  cfc11f = col_double(),
  cfc11qc = col_double(),
  cfc12 = col_double(),
  pcfc12 = col_double(),
  cfc12f = col_double(),
  cfc12qc = col_double(),
  cfc113 = col_double(),
  pcfc113 = col_double(),
  cfc113f = col_double(),
  cfc113qc = col_double(),
  ccl4 = col_double(),
  pccl4 = col_double(),
  ccl4f = col_double(),
  ccl4qc = col_double(),
  sf6 = col_double(),
  psf6 = col_double(),
  sf6f = col_double(),
  c13 = col_double(),
  c13f = col_integer(),
  c14 = col_double(),
  c14f = col_integer(),
  c14err = col_double(),
  h3 = col_double(),
  h3f = col_double(),
  h3err = col_double(),
  he3 = col_double(),
  he3f = col_double(),
  he3err = col_double(),
  he = col_double(),
  hef = col_double(),
  heerr = col_double(),
  neon = col_double(),
  neonf = col_double(),
  neonerr = col_double(),
  o18 = col_double(),
  o18f = col_double(),
  toc = col_double(),
  tocf = col_double(),
  doc = col_double(),
  docf = col_double(),
  don = col_double(),
  donf = col_double(),
  tdn = col_double(),
  tdnf = col_double(),
  chla = col_double(),
  chlaf = col_double()
)

glodap <- read_csv(here("data/glodapv2MMF.csv"), col_types = colspec, na = c("NA", "-9999"))
expcodes <- read_delim(here("data/expocodes.txt"), 
                       delim = "\t", 
                       col_names = c("num", "cruise"))
nosams <- read_csv(here("data/nosams_clivar.csv"))

# pick what we need from GLODAP
glodap_dic <- glodap %>%
  filter(!is.na(c14) | !is.na(c13)) %>%
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
