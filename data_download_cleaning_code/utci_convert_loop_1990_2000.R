library(ncdf4)
library(tidyverse)
library(imputeTS)
library(lubridate)
library(data.table)
setwd("C:/Users/Kaifs/OneDrive/Documents/dropbox_penn/Dropbox/utci_china_1982_2020")

dataFiles1990_01 <- lapply(Sys.glob("1990_01_utci/c7c48ef9-5619-4759-a51b-598464e83e88-ECMWF_utci_*.nc"), nc_open)
dataFiles1990_02 <- lapply(Sys.glob("1990_02_utci/97dd92f8-0aa2-4db9-8ed1-b8a646ed8336-ECMWF_utci_*.nc"), nc_open)
dataFiles1990_03 <- lapply(Sys.glob("1990_03_utci/d42a8857-48b8-4517-a376-2bc67ce2b256-ECMWF_utci_*.nc"), nc_open)
dataFiles1990_04 <- lapply(Sys.glob("1990_04_utci/5bc72e54-c6d6-4b6b-a25c-69452b3db85b-ECMWF_utci_*.nc"), nc_open)
dataFiles1990_05 <- lapply(Sys.glob("1990_05_utci/0b7b71d4-605f-48e0-bd1e-b8ff4878cc52-ECMWF_utci_*.nc"), nc_open)
dataFiles1990_06 <- lapply(Sys.glob("1990_06_utci/d62da7b3-4a23-45c3-844f-ca9431e10a23-ECMWF_utci_*.nc"), nc_open)
dataFiles1990_07 <- lapply(Sys.glob("1990_07_utci/b9006cbe-5d27-424c-8de6-7c83bab3a88e-ECMWF_utci_*.nc"), nc_open)
dataFiles1990_08 <- lapply(Sys.glob("1990_08_utci/b2f44641-fc26-4831-87d0-20573da852a2-ECMWF_utci_*.nc"), nc_open)
dataFiles1990_09 <- lapply(Sys.glob("1990_09_utci/67651da4-3109-4e28-b13d-889c0f8205dc-ECMWF_utci_*.nc"), nc_open)
dataFiles1990_10 <- lapply(Sys.glob("1990_10_utci/6a2943ad-78ee-483c-9bb5-f4524217422c-ECMWF_utci_*.nc"), nc_open)
dataFiles1990_11 <- lapply(Sys.glob("1990_11_utci/6e7e9f95-f7a8-402f-ac84-64b9f5ed9d7a-ECMWF_utci_*.nc"), nc_open)
dataFiles1990_12 <- lapply(Sys.glob("1990_12_utci/d0d7312c-b37a-4745-84a6-beed9eb8677f-ECMWF_utci_*.nc"), nc_open)
dataFiles2000_01 <- lapply(Sys.glob("2000_01_utci/ea7625ba-ab7e-4982-b461-6cc8dd828e6b-ECMWF_utci_*.nc"), nc_open)
dataFiles2000_02 <- lapply(Sys.glob("2000_02_utci/42daa955-4ef8-4a42-b8b4-454f6ad9771d-ECMWF_utci_*.nc"), nc_open)
dataFiles2000_03 <- lapply(Sys.glob("2000_03_utci/bc8c80cc-0d9a-45cf-8058-889f46c54bd9-ECMWF_utci_*.nc"), nc_open)
dataFiles2000_04 <- lapply(Sys.glob("2000_04_utci/0dd7c55d-37d3-40da-9c17-73cd3f3257f4-ECMWF_utci_*.nc"), nc_open)
dataFiles2000_05 <- lapply(Sys.glob("2000_05_utci/c6295349-265c-4b58-8aa6-cd340978c232-ECMWF_utci_*.nc"), nc_open)
dataFiles2000_06 <- lapply(Sys.glob("2000_06_utci/d8b38106-7ca6-49d3-896e-1e6be674234a-ECMWF_utci_*.nc"), nc_open)
dataFiles2000_07 <- lapply(Sys.glob("2000_07_utci/01fbd388-60cc-46fa-ade9-937a4d0b67a3-ECMWF_utci_*.nc"), nc_open)
dataFiles2000_08 <- lapply(Sys.glob("2000_08_utci/d351f3ec-b44a-4530-a3d9-6eb6d49ff416-ECMWF_utci_*.nc"), nc_open)
dataFiles2000_09 <- lapply(Sys.glob("2000_09_utci/0a6f60ff-d576-405a-b6ef-d6f6007b13dd-ECMWF_utci_*.nc"), nc_open)
dataFiles2000_10 <- lapply(Sys.glob("2000_10_utci/0f03316c-4e6e-4bdc-a8af-38995abd8845-ECMWF_utci_*.nc"), nc_open)
dataFiles2000_11 <- lapply(Sys.glob("2000_11_utci/5a62c4e6-8b45-4b9f-906a-e2d668b7319e-ECMWF_utci_*.nc"), nc_open)
dataFiles2000_12 <- lapply(Sys.glob("2000_12_utci/da59b4a9-8ab7-4425-9e34-0d68e5748cae-ECMWF_utci_*.nc"), nc_open)
# 

dataFiles1990 <- list(dataFiles1990_01,dataFiles1990_02,dataFiles1990_03,dataFiles1990_04,dataFiles1990_05,dataFiles1990_06,
                      dataFiles1990_07,dataFiles1990_08,dataFiles1990_09,dataFiles1990_10,dataFiles1990_11,dataFiles1990_12)

dataFiles2000 <- list(dataFiles2000_01,dataFiles2000_02,dataFiles2000_03,dataFiles2000_04,dataFiles2000_05,dataFiles2000_06,
                      dataFiles2000_07,dataFiles2000_08,dataFiles2000_09,dataFiles2000_10,dataFiles2000_11,dataFiles2000_12)

utci_ave_1990 <- list(vector(mode='list', length=31), vector(mode='list', length=28),vector(mode='list', length=31),
                      vector(mode='list', length=30),vector(mode='list', length=31),vector(mode='list', length=30),
                      vector(mode='list', length=31),vector(mode='list', length=31),vector(mode='list', length=30),
                      vector(mode='list', length=31),vector(mode='list', length=30),vector(mode='list', length=31))

utci_ave_1990_hour <- list(vector(mode='list', length=31), vector(mode='list', length=28),vector(mode='list', length=31),
                      vector(mode='list', length=30),vector(mode='list', length=31),vector(mode='list', length=30),
                      vector(mode='list', length=31),vector(mode='list', length=31),vector(mode='list', length=30),
                      vector(mode='list', length=31),vector(mode='list', length=30),vector(mode='list', length=31))

utci_ave_2000 <- list(vector(mode='list', length=31), vector(mode='list', length=28),vector(mode='list', length=31),
                      vector(mode='list', length=30),vector(mode='list', length=31),vector(mode='list', length=30),
                      vector(mode='list', length=31),vector(mode='list', length=31),vector(mode='list', length=30),
                      vector(mode='list', length=31),vector(mode='list', length=30),vector(mode='list', length=31))

utci_ave_2000_hour <- list(vector(mode='list', length=31), vector(mode='list', length=28),vector(mode='list', length=31),
                      vector(mode='list', length=30),vector(mode='list', length=31),vector(mode='list', length=30),
                      vector(mode='list', length=31),vector(mode='list', length=31),vector(mode='list', length=30),
                      vector(mode='list', length=31),vector(mode='list', length=30),vector(mode='list', length=31))


#utci_hour_1990 <- vector(mode='list',length=31)


################################################################################
# 1990
for (t in 1:12) {
  for (i in 1:length(utci_ave_1990[[t]])) {
    
    our_nc_data <- dataFiles1990[[t]][[i]]
    
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
      utci[,,m] <- na_interpolation(utci[,,m], option = "linear")
    }
    
    
    lonlattime <- as.matrix(expand.grid(lon,lat,time)) # this might take several seconds
    #reshape whole utci array
    utci_vec_long <- as.vector(utci)
    length(utci) 
    #Create data.frame
    utci_obs <- data.frame(cbind(lonlattime, utci_vec_long))
    
    colnames(utci_obs) <- c('Long','Lat','hour','utci_Kelvin')
    utci_obs$utci_celsisu <- utci_obs$utci_Kelvin - 273.15
    
    utci_obs_hour <- utci_obs %>%
      select(-utci_Kelvin) %>%
      mutate(
        year = 1990,
        month = t,
        day = i,
        date_hour = make_datetime(year, month, day, hour)
      )
    
    print(t)
    print(i)
    
    utci_ave_1990_hour[[t]][[i]] <- utci_obs_hour
    
#daily average      
    # utci_obs_ave <- utci_obs %>% 
    #   group_by(Long,Lat) %>%
    #   summarise(utci_ave = mean(utci_celsisu)) %>%
    #   mutate(
    #     year = "1990",
    #     month = t,
    #     day = i,
    #     date = as.Date(with(.,paste(year,month,day,sep="-")),"%Y-%m-%d")
    #   )
    
    # utci_ave_1990[[t]][[i]] <- utci_obs_ave
  }
}



################################################################################
# 2000

for (t in 1:12) {
  for (i in 1:length(utci_ave_2000[[t]])) {
    
    our_nc_data <- dataFiles2000[[t]][[i]]
    
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
      utci[,,m] <- na_interpolation(utci[,,m], option = "linear")
    }
    
    
    lonlattime <- as.matrix(expand.grid(lon,lat,time)) # this might take several seconds
    #reshape whole utci array
    utci_vec_long <- as.vector(utci)
    length(utci) 
    #Create data.frame
    utci_obs <- data.frame(cbind(lonlattime, utci_vec_long))
    
    colnames(utci_obs) <- c('Long','Lat','hour','utci_Kelvin')
    utci_obs$utci_celsisu <- utci_obs$utci_Kelvin - 273.15
    utci_obs_hour <- utci_obs %>%
      select(-utci_Kelvin) %>%
      mutate(
        year = 2000,
        month = t,
        day = i,
        date_hour = make_datetime(year, month, day, hour)
      )
    
    print(t)
    print(i)
    
    utci_ave_2000_hour[[t]][[i]] <- utci_obs_hour
    
#daily average    
    # utci_obs_ave <- utci_obs %>% 
    #   group_by(Long,Lat) %>%
    #   summarise(utci_ave = mean(utci_celsisu)) %>%
    #   mutate(
    #     year = "2000",
    #     month = t,
    #     day = i,
    #     date = as.Date(with(.,paste(year,month,day,sep="-")),"%Y-%m-%d")
    #   )
    # 
    # utci_ave_2000[[t]][[i]] <- utci_obs_ave
  }
}





#utci_ave_1990_wide <- lapply(utci_ave_1990, rbindlist)
#utci_ave_1990_wide <- do.call(rbind, utci_ave_1990_wide)

# utci_ave_2000_wide <- lapply(utci_ave_2000, rbindlist)
# utci_ave_2000_wide <- do.call(rbind, utci_ave_2000_wide)
# 
# utci_ave_1990_wide <- 
#   utci_ave_1990_wide %>% 
#   select(-year,-month,-day) %>%
#   pivot_wider(names_from = "date",values_from = "utci_ave")
# 
# utci_ave_2000_wide <- 
#   utci_ave_2000_wide %>% 
#   select(-year,-month,-day) %>%
#   pivot_wider(names_from = "date",values_from = "utci_ave")
# 
# 
# write.csv(utci_ave_1990_wide,"utci_ave_1990.csv")
# write.csv(utci_ave_2000_wide,"utci_ave_2000.csv")

rm(list = setdiff(ls(), "utci_ave_1990_hour"))
utci_ave_1990_wide_hour <- lapply(utci_ave_1990_hour, rbindlist)
rm(list = setdiff(ls(), "utci_ave_1990_wide_hour"))
utci_ave_1990_wide_hour <- do.call(rbind, utci_ave_1990_wide_hour)



utci_ave_1990_wide_hour <- 
  utci_ave_1990_wide_hour %>% 
  select(-year,-month,-day,-hour) %>%
  pivot_wider(names_from = "date_hour",values_from = "utci_celsisu")

write.csv(utci_ave_1990_wide_hour,"utci_ave_1990_hour.csv")







rm(list = setdiff(ls(), "utci_ave_2000_hour"))
utci_ave_2000_wide_hour <- lapply(utci_ave_2000_hour, rbindlist)
rm(list = setdiff(ls(), "utci_ave_2000_wide_hour"))
utci_ave_2000_wide_hour <- do.call(rbind, utci_ave_2000_wide_hour)


utci_ave_2000_wide_hour <- 
  utci_ave_2000_wide_hour %>% 
  select(-year,-month,-day,-hour) %>%
  pivot_wider(names_from = "date_hour",values_from = "utci_celsisu")

write.csv(utci_ave_2000_wide_hour,"utci_ave_2000_hour.csv")
