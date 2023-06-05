library(ncdf4)
library(tidyverse)
setwd("C:/Users/Kaifs/OneDrive/Documents/dropbox_penn/Dropbox/PIRE/team/kai_feng/utci_ecmwf_raw_data")


dataFiles2010_01 <- lapply(Sys.glob("2010_01_utci/cfca7a79-b969-456c-96ae-7ce9447abe36-ECMWF_utci_*.nc"), nc_open)
dataFiles2010_02 <- lapply(Sys.glob("2010_02_utci/5fc6aac6-70fb-471b-bf69-6143741eec59-ECMWF_utci_*.nc"), nc_open)
dataFiles2010_03 <- lapply(Sys.glob("2010_03_utci/3bc0112c-b7b7-47e0-83bf-8544870ebf8f-ECMWF_utci_*.nc"), nc_open)
dataFiles2010_04 <- lapply(Sys.glob("2010_04_utci/90f28dd2-561e-484b-8a2d-a2b0fb8a16e3-ECMWF_utci_*.nc"), nc_open)
dataFiles2010_05 <- lapply(Sys.glob("2010_05_utci/3dd5c4b4-98ea-4b28-bf43-264d2af3f572-ECMWF_utci_*.nc"), nc_open)
dataFiles2010_06 <- lapply(Sys.glob("2010_06_utci/a3420cb0-f8e7-497b-81f7-564cd073bf54-ECMWF_utci_*.nc"), nc_open)
dataFiles2010_07 <- lapply(Sys.glob("2010_07_utci/053242fc-f508-411a-9ce3-84ab8225bce3-ECMWF_utci_*.nc"), nc_open)
dataFiles2010_08 <- lapply(Sys.glob("2010_08_utci/27df6ae3-c8b8-4a72-aed3-47023bca0c53-ECMWF_utci_*.nc"), nc_open)
dataFiles2010_09 <- lapply(Sys.glob("2010_09_utci/ee2027fa-676e-4f98-8790-8d6a7e0f70ab-ECMWF_utci_*.nc"), nc_open)
dataFiles2010_10 <- lapply(Sys.glob("2010_10_utci/03151b5f-8674-4c85-a618-b520106d2cd9-ECMWF_utci_*.nc"), nc_open)
dataFiles2010_11 <- lapply(Sys.glob("2010_11_utci/346296e7-a1a7-4225-9121-cb758944add7-ECMWF_utci_*.nc"), nc_open)
dataFiles2010_12 <- lapply(Sys.glob("2010_12_utci/92601528-080a-4d1b-a325-85252395c33c-ECMWF_utci_*.nc"), nc_open)
dataFiles2020_01 <- lapply(Sys.glob("2020_01_utci/ab581cc9-12b8-4741-bf3f-676fda6c6d44-ECMWF_utci_*.nc"), nc_open)
dataFiles2020_02 <- lapply(Sys.glob("2020_02_utci/4ac07964-7216-4e8c-9595-ce2d33300bc2-ECMWF_utci_*.nc"), nc_open)
dataFiles2020_03 <- lapply(Sys.glob("2020_03_utci/839982e9-2183-4091-88ae-048719827704-ECMWF_utci_*.nc"), nc_open)
dataFiles2020_04 <- lapply(Sys.glob("2020_04_utci/124a6bd1-ad12-4c7d-83f8-e82c2415860c-ECMWF_utci_*.nc"), nc_open)
dataFiles2020_05 <- lapply(Sys.glob("2020_05_utci/c450252f-e022-4ce2-af84-2bd0f246077f-ECMWF_utci_*.nc"), nc_open)
dataFiles2020_06 <- lapply(Sys.glob("2020_06_utci/92f7e348-13e3-4013-9336-2e6392c4f499-ECMWF_utci_*.nc"), nc_open)
dataFiles2020_07 <- lapply(Sys.glob("2020_07_utci/3d915572-5cbb-4eb0-8ff7-b677af13b616-ECMWF_utci_*.nc"), nc_open)
dataFiles2020_08 <- lapply(Sys.glob("2020_08_utci/7a156b9b-b0ff-48ff-ba13-8330a8ac7911-ECMWF_utci_*.nc"), nc_open)
dataFiles2020_09 <- lapply(Sys.glob("2020_09_utci/643777cb-bdd3-42d6-a5d0-1b67cb8d45dd-ECMWF_utci_*.nc"), nc_open)
dataFiles2020_10 <- lapply(Sys.glob("2020_10_utci/9e69b351-0657-4a4a-8fe2-9647163de89d-ECMWF_utci_*.nc"), nc_open)
dataFiles2020_11 <- lapply(Sys.glob("2020_11_utci/f6a7d16b-6be4-467e-b652-fbb11ca439bc-ECMWF_utci_*.nc"), nc_open)
dataFiles2020_12 <- lapply(Sys.glob("2020_12_utci/2c908b92-f689-4a13-b523-5266ac833dfc-ECMWF_utci_*.nc"), nc_open)


dataFiles2010 <- list(dataFiles2010_01,dataFiles2010_02,dataFiles2010_03,dataFiles2010_04,dataFiles2010_05,dataFiles2010_06,
                  dataFiles2010_07,dataFiles2010_08,dataFiles2010_09,dataFiles2010_10,dataFiles2010_11,dataFiles2010_12)

dataFiles2020 <- list(dataFiles2020_01,dataFiles2020_02,dataFiles2020_03,dataFiles2020_04,dataFiles2020_05,dataFiles2020_06,
                      dataFiles2020_07,dataFiles2020_08,dataFiles2020_09,dataFiles2020_10,dataFiles2020_11,dataFiles2020_12)

utci_ave_2010 <- list(vector(mode='list', length=31), vector(mode='list', length=28),vector(mode='list', length=31),
                      vector(mode='list', length=30),vector(mode='list', length=31),vector(mode='list', length=30),
                      vector(mode='list', length=31),vector(mode='list', length=31),vector(mode='list', length=30),
                      vector(mode='list', length=31),vector(mode='list', length=30),vector(mode='list', length=31))

utci_ave_2020 <- list(vector(mode='list', length=31), vector(mode='list', length=28),vector(mode='list', length=31),
                      vector(mode='list', length=30),vector(mode='list', length=31),vector(mode='list', length=30),
                      vector(mode='list', length=31),vector(mode='list', length=31),vector(mode='list', length=30),
                      vector(mode='list', length=31),vector(mode='list', length=30),vector(mode='list', length=31))
#utci_hour_2010 <- vector(mode='list',length=31)


################################################################################
# 2010
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


lonlattime <- as.matrix(expand.grid(lon,lat,time)) # this might take several seconds
#reshape whole utci array
utci_vec_long <- as.vector(utci)
length(utci) 
#Create data.frame
utci_obs <- data.frame(cbind(lonlattime, utci_vec_long))

colnames(utci_obs) <- c('Long','Lat','hour','utci_Kelvin')
utci_obs$utci_celsisu <- utci_obs$utci_Kelvin - 273.15
#utci_obs <- utci_obs %>%
#  mutate(strong_heat_above = case_when(utci_celsisu > 32 ~ 1,
#                                       utci_celsisu <= 32 ~ 0),
#         moderate_heat_above = case_when(utci_celsisu > 26 ~ 1,
#                                         utci_celsisu <= 26 ~ 0),
#         very_heat_above = case_when(utci_celsisu > 38 ~ 1,
#                                     utci_celsisu <= 38 ~ 0),
#         strong_cold_below = case_when(utci_celsisu < -13 ~ 1,
#                                       utci_celsisu >= -13 ~ 0),
#         very_cold_below = case_when(utci_celsisu < -27 ~ 1,
#                                     utci_celsisu >= -27 ~ 0),
#         moderate_cold_below = case_when(utci_celsisu < 0 ~ 1,
#                                         utci_celsisu >= 0  ~ 0))

utci_obs_ave <- utci_obs %>% 
  group_by(Long,Lat) %>%
  summarise(utci_ave = mean(utci_celsisu)) %>%
  mutate(
  # strong_heat_above = case_when(utci_ave > 32 ~ 1,
  #                                     utci_ave <= 32 ~ 0),
  #       moderate_heat_above = case_when(utci_ave > 26 ~ 1,
  #                                       utci_ave <= 26 ~ 0),
  #       very_strong_above = case_when(utci_ave > 38 ~ 1,
  #                                     utci_ave <= 38 ~ 0),
  #       strong_cold_below = case_when(utci_ave < -13 ~ 1,
  #                                     utci_ave >= -13 ~ 0),
  #       very_cold_below = case_when(utci_ave < -27 ~ 1,
  #                                   utci_ave >= -27 ~ 0),
  #       moderate_cold_below = case_when(utci_ave < 0 ~ 1,
  #                                       utci_ave >= 0  ~ 0),
         year = "2010",
         month = t,
         day = i,
         date = as.Date(with(.,paste(year,month,day,sep="-")),"%Y-%m-%d")
         )

utci_ave_2010[[t]][[i]] <- utci_obs_ave
  }
  }

################################################################################
# 2020

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
    
    
    lonlattime <- as.matrix(expand.grid(lon,lat,time)) # this might take several seconds
    #reshape whole utci array
    utci_vec_long <- as.vector(utci)
    length(utci) 
    #Create data.frame
    utci_obs <- data.frame(cbind(lonlattime, utci_vec_long))
    
    colnames(utci_obs) <- c('Long','Lat','hour','utci_Kelvin')
    utci_obs$utci_celsisu <- utci_obs$utci_Kelvin - 273.15
    #utci_obs <- utci_obs %>%
    #  mutate(strong_heat_above = case_when(utci_celsisu > 32 ~ 1,
    #                                       utci_celsisu <= 32 ~ 0),
    #         moderate_heat_above = case_when(utci_celsisu > 26 ~ 1,
    #                                         utci_celsisu <= 26 ~ 0),
    #         very_heat_above = case_when(utci_celsisu > 38 ~ 1,
    #                                     utci_celsisu <= 38 ~ 0),
    #         strong_cold_below = case_when(utci_celsisu < -13 ~ 1,
    #                                       utci_celsisu >= -13 ~ 0),
    #         very_cold_below = case_when(utci_celsisu < -27 ~ 1,
    #                                     utci_celsisu >= -27 ~ 0),
    #         moderate_cold_below = case_when(utci_celsisu < 0 ~ 1,
    #                                         utci_celsisu >= 0  ~ 0))
    
    utci_obs_ave <- utci_obs %>% 
      group_by(Long,Lat) %>%
      summarise(utci_ave = mean(utci_celsisu)) %>%
      mutate(
        # strong_heat_above = case_when(utci_ave > 32 ~ 1,
        #                                     utci_ave <= 32 ~ 0),
        #       moderate_heat_above = case_when(utci_ave > 26 ~ 1,
        #                                       utci_ave <= 26 ~ 0),
        #       very_strong_above = case_when(utci_ave > 38 ~ 1,
        #                                     utci_ave <= 38 ~ 0),
        #       strong_cold_below = case_when(utci_ave < -13 ~ 1,
        #                                     utci_ave >= -13 ~ 0),
        #       very_cold_below = case_when(utci_ave < -27 ~ 1,
        #                                   utci_ave >= -27 ~ 0),
        #       moderate_cold_below = case_when(utci_ave < 0 ~ 1,
        #                                       utci_ave >= 0  ~ 0),
        year = "2020",
        month = t,
        day = i,
        date = as.Date(with(.,paste(year,month,day,sep="-")),"%Y-%m-%d")
      )
    
    utci_ave_2020[[t]][[i]] <- utci_obs_ave
  }
}



################################################################################

library(data.table)

utci_ave_2010_wide <- lapply(utci_ave_2010, rbindlist)
utci_ave_2010_wide <- do.call(rbind, utci_ave_2010_wide)
utci_ave_2020_wide <- lapply(utci_ave_2020, rbindlist)
utci_ave_2020_wide <- do.call(rbind, utci_ave_2020_wide)

utci_ave_2010_wide <- 
  utci_ave_2010_wide %>% 
  select(-year,-month,-day) %>%
  pivot_wider(names_from = "date",values_from = "utci_ave")
  
utci_ave_2020_wide <- 
  utci_ave_2020_wide %>% 
  select(-year,-month,-day) %>%
  pivot_wider(names_from = "date",values_from = "utci_ave")


write.csv(utci_ave_2010_wide,"utci_ave_2010.csv")
write.csv(utci_ave_2020_wide,"utci_ave_2020.csv")










