# This script identifies and summarizes heat spells from hourly temperature data.
# It processes each location’s data, detects continuous periods exceeding a 
# temperature threshold, and computes the proportion of heat hours and heat-spell hours.
#
# HOW TO USE:
# 1. Specify file paths for your key location coordinates ('key_path') and data ('ml_path'; in this case either a cleaned CSV file or an RDA file).
# 2. Adjust parameters in `run_full_analysis()`:
#       - temp_threshold: temperature threshold over which a location-hour is observed
#       - cont_threshold: minimum consecutive hours defining a heat spell
# 3. Run to get a summary table (`heat_results`).
# 4. The script prints 3 randomly sampled locations’ results 
#
# OUTPUT x3:
#   location_id | temp_threshold | cont_threshold | prop_TTM (proportion of hours where temperature is over threshold in a location) | prop_TSM (proportion of hours within a threshold spell in a location )

# Additional code also provided at end to map results
#========================================================#

library(tidyverse)
library(reticulate)
reticulate::py_install("scipy")


#------- Load and prepare data --------
load_rda_data <- function(path) {
  env <- new.env()
  obj_names <- load(path, envir = env)
  env[[obj_names[1]]]
}

key_path <- "/Users/mlaghi/Dropbox/PIRE/team/marco_laghi/PrjCECReplicate/clean_data/df_key_loc_china_coord2county_1990.csv"
ml_path  <- "/Users/mlaghi/Dropbox/PIRE/team/marco_laghi/PrjCECReplicate/clean_data/df_era5_utci_china_1989_1991_hour.csv"

key <- read_csv(key_path) %>%
  distinct(Long, Lat, location_id)

# Comment out depending on if the data used is a raw RDA or is a cleaner CSV
#ml <- load_rda_data(ml_path)
ml <- read_csv(ml_path)

#-------- RDA cleaning if needed -------
#rename_ml_data <- function(df, key, location_col = c("Long", "Lat"), keep_col = "location_id") {
#  df %>%
#    left_join(key, by = location_col) %>%
#    filter(!is.na(.data[[keep_col]])) %>%
#    relocate(.data[[keep_col]]) %>%
#    select(-all_of(location_col))
#}

#ml <- rename_ml_data(ml, key)

#------- Reshape and set threshold --------
#change column labels as appropriate if needed
reshape_hourly <- function(df_row) {
  df_row %>%
    pivot_longer(cols = starts_with("day"), names_to = "hour", values_to = "value") %>%
    mutate(hour = as.integer(str_remove(hour, "day")),
           day = ceiling(hour / 24),
           hour_in_day = ((hour - 1) %% 24) + 1) %>%
    select(location_id, day, hour_in_day, value) %>%
    pivot_wider(names_from = hour_in_day, values_from = value, names_prefix = "hour_")
}

threshold_matrix <- function(df, temp_threshold) {
  df %>%
    mutate(across(starts_with("hour_"), ~ as.integer(.x > temp_threshold))) %>%
    { attr(., "temp_threshold") <- temp_threshold; . }
}

#------- Identify heat spells --------
temp_spell_matrix <- function(TTM, cont_threshold) {
  ndimage <- import("scipy.ndimage")

  hour_mat <- as.matrix(select(TTM, starts_with("hour_")), drop = FALSE)
  labeled <- ndimage$label(r_to_py(hour_mat))
  labeled_array <- py_to_r(labeled[[1]])

  counts <- as.data.frame(table(as.vector(labeled_array))) %>%
    filter(Var1 != "0") %>%
    mutate(Var1 = as.integer(Var1), Freq = as.integer(Freq))

  valid_labels <- counts$Var1[counts$Freq >= cont_threshold]
TSM_matrix <- matrix(
  ifelse(labeled_array %in% valid_labels, 1L, 0L),
  nrow = nrow(labeled_array),
  ncol = ncol(labeled_array)
)
  colnames(TSM_matrix) <- colnames(hour_mat)

  TSM <- bind_cols(select(TTM, location_id, day), as.data.frame(TSM_matrix))
  attr(TSM, "cont_threshold") <- cont_threshold
  TSM
}

#------- Compute proportions --------
compute_TTM_JTSM_proportions <- function(TTM, TSM) {
  ttm_hours <- select(TTM, starts_with("hour_"))
  tsm_hours <- select(TSM, starts_with("hour_"))

  prop_TTM <- mean(as.matrix(ttm_hours), na.rm = TRUE)
  prop_TSM <- mean(as.matrix(tsm_hours), na.rm = TRUE)

  tibble(
    location_id = TTM$location_id[1],
    temp_threshold = attr(TTM, "temp_threshold"),
    cont_threshold = attr(TSM, "cont_threshold"),
    prop_TTM = prop_TTM,
    prop_TSM = prop_TSM
  )
}

#------- Run for all locations --------
run_full_analysis <- function(df, temp_threshold, cont_threshold) {
  results <- vector("list", nrow(df))

  for (i in seq_len(nrow(df))) {
    df_row <- df[i, , drop = FALSE]
    reshaped <- reshape_hourly(df_row)
    TTM <- threshold_matrix(reshaped, temp_threshold)
    JTSM <- temp_spell_matrix(TTM, cont_threshold)
    results[[i]] <- compute_TTM_JTSM_proportions(TTM, JTSM)
  }

  bind_rows(results)
}

#------- Execute pipeline --------

heat_results <- run_full_analysis(ml, temp_threshold = 32, cont_threshold = 3)

#------- Display 3 random sample outputs --------
random3 <- sample_n(heat_results, 3)
random3

# If you want your sample to be in a certain range
#random3 <- sample_n(
#  heat_results %>%
#    filter(prop_TTM > 0.01 & prop_TSM > 00.001),
#  3
#)
#random3



# A

#---------- Mapping ------
library(ggplot2)
library(sf)
library(viridis)

shp <- st_read("./Dropbox/PIRE/team/marco_laghi/1990a/china90a.shp")
china_boundary <- st_union(shp)   
st_crs(china_boundary) <- 4326

key <- read_csv(key_path) %>%
  distinct(Long, Lat, location_id)
highlight_ids <- random3$location_id

heatLongLat <- heat_results  %>% 
  left_join(key , by = "location_id")

heat_sf <- st_as_sf(
  heatLongLat,
  coords = c("Long", "Lat"),
  crs = 4326
)

map_ttm <- ggplot() +
  geom_sf(data = china_boundary, fill = "grey95", color = "grey70", size = 0.2) +
  geom_sf(data = heat_sf, aes(color = prop_TTM), size = 1.4, alpha = 0.9) +
  geom_sf(data = subset(heat_sf, location_id %in% highlight_ids),
          color = "#00ff08", size = 6, shape = 2, fill = NA, stroke = 1) +
  scale_color_viridis_c(option = "C") +
  labs(title = "Proportion of Hours Above 32 UTCI Threshold (TTM)",
       color = "TTM") +
  theme_minimal()


map_tsm <- ggplot() +
  geom_sf(data = china_boundary, fill = "grey95", color = "grey70", size = 0.2) +
  geom_sf(data = heat_sf, aes(color = prop_TSM), size = 1.4, alpha = 0.9) +
  geom_sf(data = subset(heat_sf, location_id %in% highlight_ids),
          color = "#00ff08", size = 6, shape = 2, fill = NA, stroke = 1) +
  scale_color_viridis_c(option = "C", na.value = "grey90") +
  labs(title = "Proportion of Hours in 3 Hour Heat Spells (TSM)",
       color = "TSM") +
  theme_minimal()


map_ttm
map_tsm
