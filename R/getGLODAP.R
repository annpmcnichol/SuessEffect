#!/usr/bin/R

# Get current glodap master dataset and save to a file

# directory for data
datadir <- "../data/"

# create temp file
temp <- tempfile()

#download data
download.file(
  "https://www.nodc.noaa.gov/archive/arc0107/0162565/2.2/data/0-data/data_product/GLODAPv2%20Merged%20Master%20File.csv.zip",
  temp)
expocodes <- read.table("https://www.nodc.noaa.gov/archive/arc0107/0162565/2.2/data/0-data/data_product/EXPOCODES.txt")

# unzip csv (this takes a while)
data <- read.csv(unz(temp, "GLODAPv2 Merged Master File.csv"))

# write csv's to data dir
write.csv(data, paste0(datadir, "data/GLODAPv2MergedMaster.csv"))
write.csv(expocodes, paste0(datadir, "data/expocodes.csv"))

# get rid of temp file
unlink(temp)