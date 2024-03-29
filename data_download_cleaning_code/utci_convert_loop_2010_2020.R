# Instruction ----
# goal: convert utci data in netCDF format from ERA5 to csv
# data input from 2010 and 2020
# structure of the raw data name from ERA5: "long-codes"-ECMWF_ucti_"yeardate", long-codes will be same for date within month, eg:
# 2010_01:cfca7a79-b969-456c-96ae-7ce9447abe36-ECMWF_utci_20100101_v1_area_subset
# manually copy the unique long-codes, 12 in total each year, read through all data by date using lapply

# Within the loop, one can either activate codes for hourly data or daily average to produce utci day by date-hour, or by date(average over 24 hours)
# One should also activate the corresponding codes that convert dataframe to csv after loop depending on usage of hourly/daily data
# The time mark refer to UTC time. In order to get the local time in China, one should add 8 hours to the listed time in the data.
# These code did not do any conversion on time.


# Package ----
library(ncdf4)
library(tidyverse)
library(imputeTS)
library(lubridate)
library(data.table)
setwd(
  "C:/Users/Kaifs/OneDrive/Documents/dropbox_penn/Dropbox/PIRE/team/kai_feng/utci_ecmwf_raw_data"
)

# Read raw data from ERA5 ----
dataFiles2010_01 <-
  lapply(
    Sys.glob(
      "2010_01_utci/cfca7a79-b969-456c-96ae-7ce9447abe36-ECMWF_utci_*.nc"
    ),
    nc_open
  )
dataFiles2010_02 <-
  lapply(
    Sys.glob(
      "2010_02_utci/5fc6aac6-70fb-471b-bf69-6143741eec59-ECMWF_utci_*.nc"
    ),
    nc_open
  )
dataFiles2010_03 <-
  lapply(
    Sys.glob(
      "2010_03_utci/3bc0112c-b7b7-47e0-83bf-8544870ebf8f-ECMWF_utci_*.nc"
    ),
    nc_open
  )
dataFiles2010_04 <-
  lapply(
    Sys.glob(
      "2010_04_utci/90f28dd2-561e-484b-8a2d-a2b0fb8a16e3-ECMWF_utci_*.nc"
    ),
    nc_open
  )
dataFiles2010_05 <-
  lapply(
    Sys.glob(
      "2010_05_utci/3dd5c4b4-98ea-4b28-bf43-264d2af3f572-ECMWF_utci_*.nc"
    ),
    nc_open
  )
dataFiles2010_06 <-
  lapply(
    Sys.glob(
      "2010_06_utci/a3420cb0-f8e7-497b-81f7-564cd073bf54-ECMWF_utci_*.nc"
    ),
    nc_open
  )
dataFiles2010_07 <-
  lapply(
    Sys.glob(
      "2010_07_utci/053242fc-f508-411a-9ce3-84ab8225bce3-ECMWF_utci_*.nc"
    ),
    nc_open
  )
dataFiles2010_08 <-
  lapply(
    Sys.glob(
      "2010_08_utci/27df6ae3-c8b8-4a72-aed3-47023bca0c53-ECMWF_utci_*.nc"
    ),
    nc_open
  )
dataFiles2010_09 <-
  lapply(
    Sys.glob(
      "2010_09_utci/ee2027fa-676e-4f98-8790-8d6a7e0f70ab-ECMWF_utci_*.nc"
    ),
    nc_open
  )
dataFiles2010_10 <-
  lapply(
    Sys.glob(
      "2010_10_utci/03151b5f-8674-4c85-a618-b520106d2cd9-ECMWF_utci_*.nc"
    ),
    nc_open
  )
dataFiles2010_11 <-
  lapply(
    Sys.glob(
      "2010_11_utci/346296e7-a1a7-4225-9121-cb758944add7-ECMWF_utci_*.nc"
    ),
    nc_open
  )
dataFiles2010_12 <-
  lapply(
    Sys.glob(
      "2010_12_utci/92601528-080a-4d1b-a325-85252395c33c-ECMWF_utci_*.nc"
    ),
    nc_open
  )
dataFiles2020_01 <-
  lapply(
    Sys.glob(
      "2020_01_utci/ab581cc9-12b8-4741-bf3f-676fda6c6d44-ECMWF_utci_*.nc"
    ),
    nc_open
  )
dataFiles2020_02 <-
  lapply(
    Sys.glob(
      "2020_02_utci/4ac07964-7216-4e8c-9595-ce2d33300bc2-ECMWF_utci_*.nc"
    ),
    nc_open
  )
dataFiles2020_03 <-
  lapply(
    Sys.glob(
      "2020_03_utci/839982e9-2183-4091-88ae-048719827704-ECMWF_utci_*.nc"
    ),
    nc_open
  )
dataFiles2020_04 <-
  lapply(
    Sys.glob(
      "2020_04_utci/124a6bd1-ad12-4c7d-83f8-e82c2415860c-ECMWF_utci_*.nc"
    ),
    nc_open
  )
dataFiles2020_05 <-
  lapply(
    Sys.glob(
      "2020_05_utci/c450252f-e022-4ce2-af84-2bd0f246077f-ECMWF_utci_*.nc"
    ),
    nc_open
  )
dataFiles2020_06 <-
  lapply(
    Sys.glob(
      "2020_06_utci/92f7e348-13e3-4013-9336-2e6392c4f499-ECMWF_utci_*.nc"
    ),
    nc_open
  )
dataFiles2020_07 <-
  lapply(
    Sys.glob(
      "2020_07_utci/3d915572-5cbb-4eb0-8ff7-b677af13b616-ECMWF_utci_*.nc"
    ),
    nc_open
  )
dataFiles2020_08 <-
  lapply(
    Sys.glob(
      "2020_08_utci/7a156b9b-b0ff-48ff-ba13-8330a8ac7911-ECMWF_utci_*.nc"
    ),
    nc_open
  )
dataFiles2020_09 <-
  lapply(
    Sys.glob(
      "2020_09_utci/643777cb-bdd3-42d6-a5d0-1b67cb8d45dd-ECMWF_utci_*.nc"
    ),
    nc_open
  )
dataFiles2020_10 <-
  lapply(
    Sys.glob(
      "2020_10_utci/9e69b351-0657-4a4a-8fe2-9647163de89d-ECMWF_utci_*.nc"
    ),
    nc_open
  )
dataFiles2020_11 <-
  lapply(
    Sys.glob(
      "2020_11_utci/f6a7d16b-6be4-467e-b652-fbb11ca439bc-ECMWF_utci_*.nc"
    ),
    nc_open
  )
dataFiles2020_12 <-
  lapply(
    Sys.glob(
      "2020_12_utci/2c908b92-f689-4a13-b523-5266ac833dfc-ECMWF_utci_*.nc"
    ),
    nc_open
  )

### combine into year----
dataFiles2010 <-
  list(
    dataFiles2010_01,
    dataFiles2010_02,
    dataFiles2010_03,
    dataFiles2010_04,
    dataFiles2010_05,
    dataFiles2010_06,
    dataFiles2010_07,
    dataFiles2010_08,
    dataFiles2010_09,
    dataFiles2010_10,
    dataFiles2010_11,
    dataFiles2010_12
  )

dataFiles2020 <-
  list(
    dataFiles2020_01,
    dataFiles2020_02,
    dataFiles2020_03,
    dataFiles2020_04,
    dataFiles2020_05,
    dataFiles2020_06,
    dataFiles2020_07,
    dataFiles2020_08,
    dataFiles2020_09,
    dataFiles2020_10,
    dataFiles2020_11,
    dataFiles2020_12
  )

## Create empty list, date nested in month, for looping ----

utci_ave_2010 <-
  list(
    vector(mode = 'list', length = 31),
    vector(mode = 'list', length = 28),
    vector(mode = 'list', length = 31),
    vector(mode = 'list', length = 30),
    vector(mode = 'list', length = 31),
    vector(mode = 'list', length = 30),
    vector(mode = 'list', length = 31),
    vector(mode = 'list', length = 31),
    vector(mode = 'list', length = 30),
    vector(mode = 'list', length = 31),
    vector(mode = 'list', length = 30),
    vector(mode = 'list', length = 31)
  )

utci_ave_2020 <-
  list(
    vector(mode = 'list', length = 31),
    vector(mode = 'list', length = 28),
    vector(mode = 'list', length = 31),
    vector(mode = 'list', length = 30),
    vector(mode = 'list', length = 31),
    vector(mode = 'list', length = 30),
    vector(mode = 'list', length = 31),
    vector(mode = 'list', length = 31),
    vector(mode = 'list', length = 30),
    vector(mode = 'list', length = 31),
    vector(mode = 'list', length = 30),
    vector(mode = 'list', length = 31)
  )
#utci_hour_2010 <- vector(mode='list',length=31)

utci_ave_2010_hour <-
  list(
    vector(mode = 'list', length = 31),
    vector(mode = 'list', length = 28),
    vector(mode = 'list', length = 31),
    vector(mode = 'list', length = 30),
    vector(mode = 'list', length = 31),
    vector(mode = 'list', length = 30),
    vector(mode = 'list', length = 31),
    vector(mode = 'list', length = 31),
    vector(mode = 'list', length = 30),
    vector(mode = 'list', length = 31),
    vector(mode = 'list', length = 30),
    vector(mode = 'list', length = 31)
  )

utci_ave_2020_hour <-
  list(
    vector(mode = 'list', length = 31),
    vector(mode = 'list', length = 28),
    vector(mode = 'list', length = 31),
    vector(mode = 'list', length = 30),
    vector(mode = 'list', length = 31),
    vector(mode = 'list', length = 30),
    vector(mode = 'list', length = 31),
    vector(mode = 'list', length = 31),
    vector(mode = 'list', length = 30),
    vector(mode = 'list', length = 31),
    vector(mode = 'list', length = 30),
    vector(mode = 'list', length = 31)
  )


# Derive key variables from netcdf file  dataFiles_year: lat, lon, time, utci ----
## loop through months and then days ----
# 2010 and 2020 are in separated sections
# output in either hourly csv or daily average csv, currently daily average codes are inactive


### 2010 #############################################################################
for (t in 1:12) {
  for (i in 1:length(utci_ave_2010[[t]])) {
    our_nc_data <- dataFiles2010[[t]][[i]]
    
    lat <- ncvar_get(our_nc_data, "lat")
    nlat <- dim(lat)
    lon <- ncvar_get(our_nc_data, "lon")
    nlon <- dim(lon)
    time <- ncvar_get(our_nc_data, "time")
    nt <- dim(time)
    nt # 24 hours
    
    #get the variable in "matrix slices"
    utci <- ncvar_get(our_nc_data, "utci")
    dim(utci)
    
    
    # one dimentional linear interpolation
    # may want to use 3 dimensions
    for (m in 1:24) {
      utci[, , m] <- na_interpolation(utci[, , m], option = "linear")
    }
    
    
    lonlattime <-
      as.matrix(expand.grid(lon, lat, time)) # this might take several seconds
    #reshape whole utci array
    utci_vec_long <- as.vector(utci)
    length(utci)
    #Create data.frame
    utci_obs <- data.frame(cbind(lonlattime, utci_vec_long))
    
    colnames(utci_obs) <- c('Long', 'Lat', 'hour', 'utci_Kelvin')
    utci_obs$utci_celsisu <- utci_obs$utci_Kelvin - 273.15
    
    #hourly data
    # utci_obs_hour <- utci_obs %>%
    #   select(-utci_Kelvin) %>%
    #   mutate(
    #     year = 2010,
    #     month = t,
    #     day = i,
    #     date_hour = make_datetime(year, month, day, hour)
    #   )
    #
    #   print(t)
    #   print(i)
    #
    # utci_ave_2010_hour[[t]][[i]] <- utci_obs_hour
    
    #average daily
    utci_obs_ave <- utci_obs %>%
      group_by(Long, Lat) %>%
      summarise(utci_ave = mean(utci_celsisu)) %>%
      mutate(
        year = "2010",
        month = t,
        day = i,
        date = as.Date(with(., paste(
          year, month, day, sep = "-"
        )), "%Y-%m-%d")
      )
    
    utci_ave_2010[[t]][[i]] <- utci_obs_ave
    
    
    
  }
}

### 2020 #############################################################################


for (t in 1:12) {
  for (i in 1:length(utci_ave_2020[[t]])) {
    our_nc_data <- dataFiles2020[[t]][[i]]
    
    lat <- ncvar_get(our_nc_data, "lat")
    nlat <- dim(lat)
    lon <- ncvar_get(our_nc_data, "lon")
    nlon <- dim(lon)
    time <- ncvar_get(our_nc_data, "time")
    nt <- dim(time)
    nt # 24 hours
    
    #get the variable in "matrix slices"
    utci <- ncvar_get(our_nc_data, "utci")
    dim(utci)
    
    # one dimentional linear interpolation
    # may want to use 3 dimensions
    for (m in 1:24) {
      utci[, , m] <- na_interpolation(utci[, , m], option = "linear")
    }
    
    
    lonlattime <-
      as.matrix(expand.grid(lon, lat, time)) # this might take several seconds
    #reshape whole utci array
    utci_vec_long <- as.vector(utci)
    length(utci)
    #Create data.frame
    utci_obs <- data.frame(cbind(lonlattime, utci_vec_long))
    
    colnames(utci_obs) <- c('Long', 'Lat', 'hour', 'utci_Kelvin')
    utci_obs$utci_celsisu <- utci_obs$utci_Kelvin - 273.15
    
    
    #hourly data
    # utci_obs_hour <- utci_obs %>%
    #   select(-utci_Kelvin) %>%
    #   mutate(
    #     year = 2020,
    #     month = t,
    #     day = i,
    #     date_hour = make_datetime(year, month, day, hour)
    #   )
    #
    # print(t)
    # print(i)
    #
    # utci_ave_2020_hour[[t]][[i]] <- utci_obs_hour
    
    
    #daily average
    utci_obs_ave <- utci_obs %>%
      group_by(Long, Lat) %>%
      summarise(utci_ave = mean(utci_celsisu)) %>%
      mutate(
        year = "2020",
        month = t,
        day = i,
        date = as.Date(with(., paste(
          year, month, day, sep = "-"
        )), "%Y-%m-%d")
      )
    
    utci_ave_2020[[t]][[i]] <- utci_obs_ave
    
  }
}



### loop end #############################################################################

# Convert data to csv ----
# data structure: column 1 and 2 are unique coordinates. column after 2 are either date or date-hour


## conversion if daily average in used ----
### 1990 and 2000 daily ----
utci_ave_2010_wide <- lapply(utci_ave_2010, rbindlist)
utci_ave_2010_wide <- do.call(rbind, utci_ave_2010_wide)
utci_ave_2020_wide <- lapply(utci_ave_2020, rbindlist)
utci_ave_2020_wide <- do.call(rbind, utci_ave_2020_wide)

utci_ave_2010_wide <-
  utci_ave_2010_wide %>%
  select(-year, -month, -day) %>%
  pivot_wider(names_from = "date", values_from = "utci_ave")

utci_ave_2020_wide <-
  utci_ave_2020_wide %>%
  select(-year, -month, -day) %>%
  pivot_wider(names_from = "date", values_from = "utci_ave")


write.csv(utci_ave_2010_wide, "utci_ave_2010.csv")
write.csv(utci_ave_2020_wide, "utci_ave_2020.csv")



## conversion if hourly data in used ----
# removes all objects from the current R environment except for the object named utci_ave_1990_hour
# the line of code (rm...) was added to clear unused memory but only save the named object to facilitate the following code

### 2010 hour----
# rm(list = setdiff(ls(), "utci_ave_2010_hour"))
# utci_ave_2010_wide_hour <- lapply(utci_ave_2010_hour, rbindlist)
# rm(list = setdiff(ls(), "utci_ave_2010_wide_hour"))
# utci_ave_2010_wide_hour <- do.call(rbind, utci_ave_2010_wide_hour)
#
#
# utci_ave_2010_wide_hour <-
#   utci_ave_2010_wide_hour %>%
#   select(-year,-month,-day,-hour) %>%
#   pivot_wider(names_from = "date_hour",values_from = "utci_celsisu")
#
# write.csv(utci_ave_2010_wide_hour,"utci_ave_2010_hour.csv")
#
### 2020 hour----
# rm(list = setdiff(ls(), "utci_ave_2020_hour"))
# utci_ave_2020_wide_hour <- lapply(utci_ave_2020_hour, rbindlist)
# rm(list = setdiff(ls(), "utci_ave_2020_wide_hour"))
# utci_ave_2020_wide_hour <- do.call(rbind, utci_ave_2020_wide_hour)
#
#
# utci_ave_2020_wide_hour <-
#   utci_ave_2020_wide_hour %>%
#   select(-year,-month,-day,-hour) %>%
#   pivot_wider(names_from = "date_hour",values_from = "utci_celsisu")
#
# write.csv(utci_ave_2020_wide_hour,"utci_ave_2020_hour.csv")
#
#
#
#
#
