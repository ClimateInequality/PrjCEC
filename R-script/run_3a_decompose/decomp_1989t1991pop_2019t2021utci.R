# packages ----

knitr::opts_chunk$set(echo = TRUE)
library(sf)
library(sp)
library(tidyverse)
library(dplyr)
library(readxl)
library(writexl)
library(stringr)



# 1990 shpfile----
shapefile1990_path = "C:/Users/Kaifs/OneDrive/Documents/dropbox_penn/Dropbox/PIRE/team/marco_laghi/1990a/china90a.shp"
df_china_census_county_1990 <-st_read(shapefile1990_path, geometry_column = "geometry") %>%
  .[,c(3,26:67)] %>% 
  rename(GBCounty=CNTYGB) %>% 
  rowwise() %>%
  mutate(A85_f = sum(c(A90054,A90056,A90058,A90060), na.rm=T),
         A85_m = sum(c(A90053,A90055,A90057,A90059), na.rm=T)) %>%
  select(-A90053,-A90054,-A90055,-A90056,-A90057,-A90058,-A90059,-A90060) #combine age groups 85 and above 


#remove three islands in south china sea, taiwan, hong kong, marco
#why nanshang belong to HuBei Prov? 
df_china_census_county_1990 <- subset(df_china_census_county_1990, 
                                      GBCounty != "469037" & GBCounty != "469038" & 
                                        GBCounty != "469039" & GBCounty != "710000" &
                                        GBCounty != "810000" & GBCounty != "820000") %>%
  st_drop_geometry() #important to drop geometry before summation


county_geo_1990 <- st_read(shapefile1990_path)[,c("CNTYGB")] %>%
  rename(GBCounty=CNTYGB)

#remove three islands in south china sea, taiwan, hong kong, marco
county_geo_1990 <- subset(county_geo_1990, 
                          GBCounty != "469037" & GBCounty != "469038" & 
                            GBCounty != "469039" & GBCounty != "710000" &
                            GBCounty != "810000" & GBCounty != "820000")

class(county_geo_1990) 
#calculate total pop 
total_pop_1990 <- sum(as.numeric(unlist(df_china_census_county_1990[,-1]), na.rm = TRUE))


#divide total pop
df_china_census_county_1990 <- df_china_census_county_1990 %>% 
  mutate_at(vars(-1), ~ . / total_pop_1990) 

check_sum <- sum(as.numeric(unlist(df_china_census_county_1990[,-1]), na.rm = TRUE))
#should be 1

colnames(df_china_census_county_1990)[2:37] <-c(
  "popgrp1",
  "popgrp2",
  "popgrp3",
  "popgrp4",
  "popgrp5",
  "popgrp6",
  "popgrp7",
  "popgrp8",
  "popgrp9",
  "popgrp10",
  "popgrp11",
  "popgrp12",
  "popgrp13",
  "popgrp14",
  "popgrp15",
  "popgrp16",
  "popgrp17",
  "popgrp18",
  "popgrp19",
  "popgrp20",
  "popgrp21",
  "popgrp22",
  "popgrp23",
  "popgrp24",
  "popgrp25",
  "popgrp26",
  "popgrp27",
  "popgrp28",
  "popgrp29",
  "popgrp30",
  "popgrp31",
  "popgrp32",
  "popgrp33",
  "popgrp34",
  "popgrp35",
  "popgrp36") 

#write.csv(df_china_census_county_1990,
#          "C:/Users/Kaifs/OneDrive/Documents/dropbox_penn/Dropbox/PIRE/team/kai_feng/clean_data/df_china_census_county_1990.csv",
#          row.names = FALSE) 

##############################################
# substitute utci_ave_1990 with utci_ave_2020
##############################################
# df_key_loc_china_coord2county_1990 ----

# Should only need to do this once, to create ave file with long, lat

locID20 <- read_csv("/Users/mlaghi/Dropbox/PIRE/team/marco_laghi/PrjCECReplicate/clean_data/df_era5_utci_china_2019_2021_hour.csv")
coord20 <- read_csv("/Users/mlaghi/Documents/GitHub/PrjCEC/data/df_key_loc_china_coord2county_2020.csv")
coord20 <- coord20 %>%
  select(Long, Lat, location_id) %>%
  left_join(locID20, by = location_id) %>%
  select(-location_id)
write_csv("/Users/mlaghi/Dropbox/PIRE/team/marco_laghi/PrjCECReplicate/clean_data/utci_ave_2019t2021_hour.csv")


#return to Kai code


utci_ave_1990_hour <- read_csv("/Users/mlaghi/Dropbox/PIRE/team/marco_laghi/PrjCECReplicate/clean_data/utci_ave_2019t2021_hour.csv") %>% select(-...1) 
utci_ave_1990_hour <- utci_ave_1990_hour %>%
  arrange(Long, Lat)

#utci_ave_1990_hour <- utci_ave_1990_wide_hour
#transform to spatial data 
utci_point_1990 <- st_as_sf(utci_ave_1990_hour,coords=c("Long","Lat"),crs=4326) 
class(utci_point_1990) 

#spatial join, keeping the county name
st_crs(utci_point_1990) <- st_crs(county_geo_1990)
county_join_1990<-st_join(utci_point_1990, left = TRUE, county_geo_1990[c("GBCounty")])# join points
# county_join_year, find point within county and assign county name(id) to point data
length(unique(county_join_1990$GBCounty)) # some counties are too small to capture any points


#find missing county
dt_mark <-as.data.frame(unique(county_join_1990$GBCounty)[-1])
colnames(dt_mark)<-"GBCounty"
dt_mark$mark<-0
dt_mark<-left_join(county_geo_1990,dt_mark)
dt_mark<-dt_mark %>% 
  select(GBCounty,mark)

dt_mark <- replace(dt_mark, is.na(dt_mark), 1)
# row with 1 in mark indicates counties that are too small/skewed to capture points 


#find nearest point for small county
#get coordinate from point
point<-utci_point_1990[,c("geometry")]
# calc centroids
regions_centr <- st_centroid(st_make_valid(county_geo_1990))
# find the nearest neighbour to the centroid of the each polygon
# create a new column with newest point
regions_centr <- as.data.frame(c(regions_centr,point[st_nearest_feature(regions_centr, point),]))
regions_centr <- left_join(regions_centr,dt_mark,by="GBCounty")

small_county_1990 <-regions_centr %>%
  filter(mark == 1)%>%
  mutate(geometry=geometry.1)%>%
  left_join(utci_point_1990)


# prepare same column names as for "county_join_1990" to join
#small_county_1990 <-small_county_1990[, c(7:372,6,1)]
small_county_1990 <-small_county_1990[, c(7:length(small_county_1990),6,1)]
small_county_1990<-st_as_sf(small_county_1990)
st_crs(county_join_1990) <- st_crs(small_county_1990)


utci_county_1990_final <- rbind(county_join_1990,small_county_1990) 


df_key_loc_china_coord2county_1990 <- utci_county_1990_final %>%
  mutate(Long = unlist(map(utci_county_1990_final$geometry,1)),
         Lat = unlist(map(utci_county_1990_final$geometry,2))) %>%
  select(Long,Lat,GBCounty) %>%
  na.omit()

length(unique(df_key_loc_china_coord2county_1990$GBCounty))
length(unique(county_geo_1990$GBCounty))
#check same length of unique county

#note that point id ...1 is not unique because of usage of nearest point
df_key_loc_china_coord2county_1990 <- st_drop_geometry(df_key_loc_china_coord2county_1990)






# df_era5_utci_china ----
# utci_ave_1990 <- utci_ave_1990 %>%
#   mutate(location_id = row_number()) %>%
#   .[,c(-1)]

utci_ave_1990_hour <- utci_ave_1990_hour %>%
  mutate(location_id = row_number()) 

key_long_lat <- df_key_loc_china_coord2county_1990 %>%
  select(Long,Lat) %>%
  unique(.)

df_era5_utci_china_1990 <- key_long_lat %>%
  left_join(utci_ave_1990_hour) %>%
  .[,c(-1,-2)] 

df_era5_utci_china_1990 <- st_drop_geometry(df_era5_utci_china_1990)

# put location_id at front
df_era5_utci_china_1990 <- df_era5_utci_china_1990[, c(length(names(df_era5_utci_china_1990)), 1:(length(names(df_era5_utci_china_1990)) - 1))]

# Change column names to "day1" to "day365"
for (i in 1:ncol(df_era5_utci_china_1990)) {
  colnames(df_era5_utci_china_1990)[i+1] <- paste("day", i, sep = "")
}


#add location id to coord2county
df_key_loc_china_coord2county_1990<-df_key_loc_china_coord2county_1990 %>%
  select(GBCounty,Long,Lat) %>%
  left_join(utci_ave_1990_hour[,c("Long","Lat","location_id")])


#write.csv(df_key_loc_china_coord2county_1990,"C:/Users/Kaifs/OneDrive/Documents/dropbox_penn/Dropbox/PIRE/team/kai_feng/clean_data/df_key_loc_china_coord2county_1990.csv",row.names = FALSE)
write.csv(df_era5_utci_china_1990,"/Users/mlaghi/Dropbox/PIRE/team/marco_laghi/PrjCECReplicate/clean_data/df_era5_utci_china_1990demo_2019t2021utci.csv",row.names = FALSE)


# # df_key_demo_china_census_1990 ----
# 
# df_key_demo_china_census_1990 <- data.frame (
#   year = "1990",
#   popgrp_key= c("popgrp1","popgrp2","popgrp3","popgrp4","popgrp5",
#                 "popgrp6","popgrp7","popgrp8","popgrp9","popgrp10",
#                 "popgrp11","popgrp12","popgrp13","popgrp14","popgrp15",
#                 "popgrp16","popgrp17","popgrp18","popgrp19","popgrp20",
#                 "popgrp21","popgrp22","popgrp23","popgrp24", "popgrp25",
#                 "popgrp26","popgrp27","popgrp28","popgrp29","popgrp30",
#                 "popgrp31","popgrp32","popgrp33","popgrp34","popgrp35","popgrp36"),
#   gender = c("male","female","male","female","male","female","male","female","male","female",
#              "male","female","male","female","male","female","male","female","male","female",
#              "male","female","male","female","male","female","male","female","male","female",
#              "male","female","male","female","male","female"),
#   age_group=c("00_04","00_04","05_09","05_09","10_14","10_14","15_19","15_19",
#               "20_24","20_24","25_29","25_29","30_34","30_34","35_39","35_39",
#               "40_44","40_44","45_49","45_49","50_54","50_54","55_59","55_59",
#               "60_64","60_64","65_69","65_69","70_74","70_74","75_79","75_79",
#               "80_84","80_84","85_85","85_85"),
#   all_groups = 1,
#   age_group_m3 = c("0_14","0_14","0_14","0_14","0_14","0_14","15_59","15_59",
#                    "15_59","15_59","15_59","15_59","15_59","15_59","15_59","15_59",
#                    "15_59","15_59","15_59","15_59","15_59","15_59","15_59","15_59",
#                    "60_85","60_85","60_85","60_85","60_85","60_85","60_85","60_85",
#                    "60_85","60_85","60_85","60_85"),
#   label = c(
#     "Total Males at 0-4 Age",
#     "Total Females at 0-4 Age",
#     "Total Males at 5-9 Age",
#     "Total Females at 5-9 Age",
#     "Total Males at 10-14 Age",
#     "Total Females at 10-14 Age",
#     "Total Males at 15-19 Age",
#     "Total Females at 15-19 Age",
#     "Total Males at 20-24 Age",
#     "Total Females at 20-24 Age",
#     "Total Males at 25-29 Age",
#     "Total Females at 25-29 Age",
#     "Total Males at 30-34 Age",
#     "Total Females at 30-34 Age",
#     "Total Males at 35-39 Age",
#     "Total Females at 35-39 Age",
#     "Total Males at 40-44 Age",
#     "Total Females at 40-44 Age",
#     "Total Males at 45-49 Age",
#     "Total Females at 45-49 Age",
#     "Total Males at 50-54 Age",
#     "Total Females at 50-54 Age",
#     "Total Males at 55-59 Age",
#     "Total Females at 55-59 Age",
#     "Total Males at 60-64 Age",
#     "Total Females at 60-64 Age",
#     "Total Males at 65-69 Age",
#     "Total Females at 65-69 Age",
#     "Total Males at 70-74 Age",
#     "Total Females at 70-74 Age",
#     "Total Males at 75-79 Age",
#     "Total Females at 75-79 Age",
#     "Total Males at 80-84 Age",
#     "Total Females at 80-84 Age",
#     "Total Males at 85 Age and over",
#     "Total Females at 85 Age and over"
#   ))
# 
# #write.csv(df_key_demo_china_census_1990,"C:/Users/Kaifs/OneDrive/Documents/dropbox_penn/Dropbox/PIRE/team/kai_feng/clean_data/df_key_demo_china_census_1990.csv",row.names = FALSE)
# 
# 
# # df_key_loc_china_county2province_1990 ----
# df_key_loc_china_county2province_1990 <- st_read(shapefile1990_path, geometry_column = "geometry") %>%
#   rename(GBCounty=CNTYGB,
#          Prov_En = EPROV) %>% 
#   st_drop_geometry() %>%
#   subset(GBCounty != "469037" & GBCounty != "469038" & 
#            GBCounty != "469039" & GBCounty != "710000" &
#            GBCounty != "810000" & GBCounty != "820000") %>%
#   select("GBCounty","Prov_En") %>%
#   mutate(all_locations=1)
# 
# #for county 420822, "shangyang gongguanju", the EPROV was labeled as "Nation". It should be labeled as "Hubei". 
# #it is a prison unit in Hubei. We relabeled it as "Hubei".
# 
# 
# #write.csv(df_key_loc_china_county2province_1990,"C:/Users/Kaifs/OneDrive/Documents/dropbox_penn/Dropbox/PIRE/team/kai_feng/clean_data/df_key_loc_china_county2province_1990.csv")
# 


