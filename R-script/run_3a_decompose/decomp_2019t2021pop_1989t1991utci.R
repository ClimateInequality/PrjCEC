#title: "Untitled"
#output: html_document
#date: '2023-07-07'

#create data for decomposition analysis

knitr::opts_chunk$set(echo = TRUE)
library(sf)
library(sp)
library(tidyverse)
library(dplyr)
library(readxl)
library(writexl)


##############################################
# substitute utci_ave_2020 with utci_ave_1990
##############################################

# Should only need to do this once, to create ave file with long, lat

locID90 <- read_csv("/Users/mlaghi/Dropbox/PIRE/team/marco_laghi/PrjCECReplicate/clean_data/df_era5_utci_china_1989_1991_hour.csv")
coord90 <- read_csv("/Users/mlaghi/Documents/GitHub/PrjCEC/data/df_key_loc_china_coord2county_1990.csv")
coord90 <- coord90 %>%
  select(Long, Lat, location_id) %>%
  left_join(locID90, by = location_id) %>%
  select(-location_id)
write_csv("/Users/mlaghi/Dropbox/PIRE/team/marco_laghi/PrjCECReplicate/clean_data/utci_ave_1989t1991_hour.csv")


#return to Kai code

# note: use 1990 utci here
utci_ave_2020 <- read_csv("/Users/mlaghi/Dropbox/PIRE/team/marco_laghi/PrjCECReplicate/clean_data/utci_ave_1989t1991_hour.csv") %>% select(-...1) %>% arrange(Long, Lat)
utci_point_2020 <- st_as_sf(utci_ave_2020,coords=c("Long","Lat"),crs=4326)
class(utci_point_2020) 

# Retrieve coordinate reference system from 2010
county_geo_2010 <- st_read("C:/Users/Kaifs/OneDrive/Documents/dropbox_penn/Dropbox/PIRE/team/kai_feng/census_county_data/2010 Census Data and GIS/Data/2010CountyCensusA_Extension.shp")[,c(1)]
# remove San Sha Cities in south china sea
county_geo_2010 <- subset(county_geo_2010, GBCounty != "469031" & GBCounty != "469032" & GBCounty != "469033")
class(county_geo_2010) 
st_crs(utci_point_2020) <- st_crs(county_geo_2010)


county2020<-st_read("C:/Users/Kaifs/OneDrive/Documents/dropbox_penn/Dropbox/PIRE/team/kai_feng/census_county_data/第七次人口普查及GDP/2020shp/2020.shp") %>%
  mutate(county=县,
         GBCounty=县代码) %>%
  mutate(GBCounty = case_when(county == "大柴旦行委" ~ 632857,
                              county == "繁昌区" ~ 340212,
                              county == "水城区" ~ 520204,
                              county == "偃师区" ~ 410381,
                              county == "加格达奇区" ~ 232761,
                              county == "监利市" ~ 421088,
                              county == "崇川区" ~ 320613,
                              county == "海门区" ~ 320614,
                              county != 9999 ~ GBCounty))

# ok to remove 钱塘区 and 临平区 from county file, established in 2021 and not included in 2020 county census
# ok to remove 新星市 from county file for the same reason
# 710000	台湾省 #810013	香港 #820004	澳门 not included for analysis
# merge rows of 港闸区崇川区 and keep 崇川区 id

census2020 <- read_xlsx("C:/Users/Kaifs/OneDrive/Documents/dropbox_penn/Dropbox/PIRE/team/kai_feng/census_county_data/census_county_2020_v1.xlsx")[,c(1:4,21:58)] %>%
  mutate(区划代码=as.numeric(区划代码),
         区县 = case_when(区县 == "港闸区" ~ "崇川区",
                        区县 != 9999 ~ 区县),
         区划代码 = case_when(区划代码 == 320611 ~ 320613,
                          区划代码 != 320611 ~ 区划代码)) %>%
  group_by(省,市,区县,区划代码) %>%
  summarise(across(everything(), sum)) %>%
  mutate(GBCounty = 区划代码)


#开发区，工业区，旅游区隶属未解决

shp_county_2020 <- left_join(county2020,census2020,by=c("GBCounty","省","市"))
df_china_census_county_2020 <- shp_county_2020[,c(15,18:55)]
df_china_census_county_2020 <- df_china_census_county_2020[complete.cases(df_china_census_county_2020$男_0岁), ]
df_china_census_county_2020<-st_drop_geometry(df_china_census_county_2020)

#calculate total pop
total_pop_2020 <- sum(as.numeric(unlist(st_drop_geometry(df_china_census_county_2020[,-1])), na.rm = TRUE))

#divide total pop
df_china_census_county_2020 <- df_china_census_county_2020 %>%
  mutate_at(vars(-1), ~ . / total_pop_2020)

check_sum <- sum(as.numeric(unlist(df_china_census_county_2020[,-1]), na.rm = TRUE))
check_sum
#should be 1


colnames(df_china_census_county_2020)[2:39] <-c(
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
  "popgrp36",
  "popgrp37",
  "popgrp38") 



#write.csv(df_china_census_county_2020,"C:/Users/Kaifs/OneDrive/Documents/dropbox_penn/Dropbox/PIRE/team/kai_feng/clean_data/df_china_census_county_2020.csv",row.names = FALSE)

df_key_loc_china_county2province_2020 <- 
  shp_county_2020 %>%
  mutate(County_CH = 县,
         GBProv=省代码,
         Prov_CH=省,
         GbCity=市代码,
         City_CH=市) %>%
  select(GBCounty,County_CH,GBProv,GbCity,City_CH,Prov_CH) %>%
  st_drop_geometry()

df_key_loc_china_county2province_2010 <- st_read("C:/Users/Kaifs/OneDrive/Documents/dropbox_penn/Dropbox/PIRE/team/kai_feng/census_county_data/2010 Census Data and GIS/Data/2010CountyCensusA_Extension.shp")[,c(1:3,521:527)]
df_key_loc_china_county2province_2010 <- st_drop_geometry(df_key_loc_china_county2province_2010)
df_key_loc_china_county2province_2010$all_locations <- 1

prov_en_label <- df_key_loc_china_county2province_2010 %>%
  select(Prov_CH,Prov_En) %>%
  distinct()

character_to_remove <- "省"
df_key_loc_china_county2province_2020$Prov_CH <- gsub(character_to_remove, "", df_key_loc_china_county2province_2020$Prov_CH)

df_key_loc_china_county2province_2020 <- df_key_loc_china_county2province_2020 %>% left_join(prov_en_label) %>%
  mutate(Prov_En = case_when(Prov_CH == "天津市" ~ "TianJin",
                             Prov_CH != "天津市" ~ Prov_En))

df_key_loc_china_county2province_2020$all_locations <- 1
#write_xlsx(df_key_loc_china_county2province_2020,"C:/Users/Kaifs/OneDrive/Documents/dropbox_penn/Dropbox/PIRE/team/kai_feng/clean_data/df_key_loc_china_county2province_2020.xlsx")
#write.csv(df_key_loc_china_county2province_2020,"C:/Users/Kaifs/OneDrive/Documents/dropbox_penn/Dropbox/PIRE/team/kai_feng/clean_data/df_key_loc_china_county2province_2020.csv")


county_geo_2020 <- shp_county_2020 %>%
  select(GBCounty)

# Update the geometry in the original polygon object
# Divide all the coordinates by 100,000
st_geometry(county_geo_2020) <- st_geometry(county_geo_2020)

class(county_geo_2020) 
#spatial join, keeping the county name
st_crs(utci_point_2020) <- st_crs(county_geo_2020)
#st_crs(utci_point_1990) <- st_crs(county_geo_2020)


county_join_2020<-st_join(utci_point_2020, left = TRUE, county_geo_2020[c("GBCounty")]) # join points


# county_join_year, find point within county and assign county name(id) to point data
length(unique(county_join_2020$GBCounty)) # some counties are too small to capture any points

dt_mark_2020 <-as.data.frame(unique(county_join_2020$GBCounty)[-1])
colnames(dt_mark_2020)<-"GBCounty"
dt_mark_2020$mark<-0
dt_mark_2020<-left_join(county_geo_2020,dt_mark_2020)
dt_mark_2020<-dt_mark_2020 %>% 
  select(GBCounty,mark)

dt_mark_2020 <- replace(dt_mark_2020, is.na(dt_mark_2020), 1)

# row with 1 in mark indicates counties that are too small/skewed to capture points 

#get coordinate from point
point_2020<-utci_point_2020[,c("geometry")]

# calc centroids
regions_centr_2020 <- st_centroid(st_make_valid(county_geo_2020))
# find the nearest neighbour to the centroid of the each polygon
# create a new column with newest point
regions_centr_2020 <- as.data.frame(c(regions_centr_2020,point_2020[st_nearest_feature(regions_centr_2020, point_2020),]))

regions_centr_2020 <- left_join(regions_centr_2020,dt_mark_2020,by="GBCounty")


small_county_2020 <-regions_centr_2020 %>%
  filter(mark == 1)%>%
  mutate(geometry=geometry.1)%>%
  left_join(utci_point_2020)

#small_county_2020 <-regions_centr %>%
#  filter(mark == 1)%>%
#  mutate(geometry=geometry.1)%>%
#  left_join(utci_point_2020)


# prepare same column names as for "county_join_2020" to join
small_county_2020 <-small_county_2020[, c(7:8766,6,1)]
small_county_2020<-st_as_sf(small_county_2020)
st_crs(county_join_2020) <- st_crs(small_county_2020)



###
utci_county_2020_final <- rbind(county_join_2020,small_county_2020) 
#utci_county_2020 <- rbind(county_join_2020,small_county_2020)
###


df_key_loc_china_coord2county_2020 <- utci_county_2020_final %>%
  mutate(Long = unlist(map(utci_county_2020_final$geometry,1)),
         Lat = unlist(map(utci_county_2020_final$geometry,2))) %>%
  select(Long,Lat,GBCounty) %>%
  na.omit()

#df_key_loc_china_coord2county_2020 <- utci_county_2020 %>%
#    mutate(Long = unlist(map(utci_county_2020$geometry,1)),
#           Lat = unlist(map(utci_county_2020$geometry,2))) %>%
#  select(...1,Long,Lat,county,county_code) %>%
#  na.omit()


length(unique(df_key_loc_china_coord2county_2020$GBCounty))
length(unique(county_geo_2020$GBCounty))
#check same length of unique county

#note that point id ...1 is not unique because of usage of nearest point

#setwd("C:/Users/Kaifs/OneDrive/Documents/dropbox_penn/Dropbox/PIRE/team/kai_feng/clean_data")

df_key_loc_china_coord2county_2020 <- st_drop_geometry(df_key_loc_china_coord2county_2020)

# note that Long Lat has duplicates because of the usage of nearest point, i.e.
# some counties share the same points


utci_ave_2020 <- utci_ave_2020 %>%
  mutate(location_id = row_number())

key_long_lat <- df_key_loc_china_coord2county_2020 %>%
  select(Long,Lat) %>%
  unique(.)

df_era5_utci_china_2020 <- key_long_lat %>%
  left_join(utci_ave_2020) %>%
  .[,c(-1,-2)] 


# put location_id at front
df_era5_utci_china_2020 <- df_era5_utci_china_2020[, c(length(names(df_era5_utci_china_2020)), 1:(length(names(df_era5_utci_china_2020)) - 1))]

# Change column names to "day1" to "day365"
for (i in 1:ncol(df_era5_utci_china_2020)) {
  colnames(df_era5_utci_china_2020)[i+1] <- paste("day", i, sep = "")
}


df_era5_utci_china_2020 <- st_drop_geometry(df_era5_utci_china_2020)

#add location id to coord2county
df_key_loc_china_coord2county_2020<-df_key_loc_china_coord2county_2020 %>%
  select(GBCounty,Long,Lat) %>%
  left_join(utci_ave_2020[,c("Long","Lat","location_id")])


#write.csv(df_key_loc_china_coord2county_2020,"C:/Users/Kaifs/OneDrive/Documents/dropbox_penn/Dropbox/PIRE/team/kai_feng/clean_data/df_key_loc_china_coord2county_2020.csv",row.names = FALSE)
write.csv(df_era5_utci_china_1990,"/Users/mlaghi/Dropbox/PIRE/team/marco_laghi/PrjCECReplicate/clean_data/df_era5_utci_china_2019t2021_hour_decomp1989t1991_new.csv",row.names = FALSE)
