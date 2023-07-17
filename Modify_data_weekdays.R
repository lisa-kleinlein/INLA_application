library(INLA)
library(spdep)
library(sf)
library(rgdal)
library(tidyverse)
library(dplyr)
library(maptools)

# mode-function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

## London
shape_london <- st_read("London-wards-2018/London-wards-2018_ESRI/London_Ward.shp")
sf_london_weekdays <- st_as_sf(london_weekdays, coords = c("lng", "lat"), crs = 4326)

# Check the CRS of shape_london and sf_london_weekdays
print(st_crs(shape_london))
print(st_crs(sf_london_weekdays))

# Align the CRS of districts and sf_london_weekdays
sf_london_weekdays <- st_transform(sf_london_weekdays, crs = st_crs(shape_london))

# Perform the spatial join
joined_data_london_weekdays <- st_join(sf_london_weekdays, shape_london)

london_weekdays_grouped <- joined_data_london_weekdays %>%
  group_by(GSS_CODE) %>%
  summarise(n = n(),
            mean_realSum = mean(realSum),
            mean_log_realSum = log(mean_realSum),
            quant_room_type_entireHomeApt = sum(room_type_entireHomeApt) / n,
            quant_room_type_privateRoom = sum(room_type_privateRoom) / n,
            quant_room_type_sharedRoom = sum(room_type_sharedRoom) / n,
            mean_room_shared = mean(room_shared),
            mean_room_shared_10 = mean(room_shared) * 10,
            mean_room_private = mean(room_private),
            mean_room_private_10 = mean(room_private) * 10,
            mean_person_capacity = mean(person_capacity),
            mean_host_is_superhost = mean(host_is_superhost),
            mean_host_is_superhost_10 = mean(host_is_superhost) * 10,
            quant_offers_2_4 = sum(offers == "2-4 offers") / n,
            quant_offers_2_4_10 = quant_offers_2_4 * 10,
            quant_offers_more_than_4 = sum(offers == "more than 4 offers") / n,
            quant_offers_more_than_4_10 = quant_offers_more_than_4 * 10,
            mean_cleanliness_rating = mean(cleanliness_rating),
            mean_guest_satisfaction_overall = mean(guest_satisfaction_overall),
            mean_guest_satisfaction_overall_10 = mean(guest_satisfaction_overall_10),
            mean_bedrooms = mean(bedrooms),
            mean_dist = mean(dist),
            mean_metro_dist = mean(metro_dist),
            mean_attr_index = mean(attr_index),
            mean_attr_index_norm = mean(attr_index_norm),
            mean_attr_index_norm_10 = mean(attr_index_norm_10),
            mean_rest_index = mean(rest_index),
            mean_rest_index_norm = mean(rest_index_norm),
            DISTRICT = getmode(DISTRICT),
            LAGSSCODE = getmode(LAGSSCODE),
            HECTARES = getmode(HECTARES),
            NONLD_AREA = getmode(NONLD_AREA),
            geometry = getmode(geometry))

cor(as.data.frame(london_weekdays_grouped)[, c("quant_room_type_entireHomeApt",
                                      "quant_room_type_privateRoom",
                                      "quant_room_type_sharedRoom",
                                      "mean_room_shared_10",
                                      "mean_room_private_10",
                                      "mean_person_capacity",
                                      "mean_host_is_superhost_10",
                                      "quant_offers_2_4_10",
                                      "quant_offers_more_than_4_10",
                                      "mean_cleanliness_rating",
                                      "mean_guest_satisfaction_overall_10",
                                      "mean_bedrooms",
                                      "mean_dist",
                                      "mean_metro_dist",
                                      "mean_attr_index",
                                      "mean_attr_index_norm_10",
                                      "mean_rest_index",
                                      "mean_rest_index_norm")])
# highly correlated pairs of variables:
# -"quant_room_type_entireHomeApt" and "quant_room_type_privateRoom"
# -"quant_room_type_privateRoom" and "mean_room_private_10"
# -"quant_room_type_sharedRoom" and "mean_room_shared_10"
# -"mean_attr_index" and "mean_attr_index_norm_10" 
# -"mean_rest_index" and "mean_rest_index_norm"
# -"mean_attr_index" and "mean_rest_index"
# -> exclude "quant_room_type_entireHomeApt", "quant_room_type_privateRoom", "quant_room_type_sharedRoom",
# "mean_guest_satisfaction_overall", "mean_attr_index", "mean_rest_index_norm", "mean_rest_index" from model

cor(as.data.frame(london_weekdays_grouped)[, c("mean_room_shared_10",
                                               "mean_room_private_10",
                                               "mean_person_capacity",
                                               "mean_host_is_superhost_10",
                                               "quant_offers_2_4_10",
                                               "quant_offers_more_than_4_10",
                                               "mean_cleanliness_rating",
                                               "mean_guest_satisfaction_overall_10",
                                               "mean_bedrooms",
                                               "mean_dist",
                                               "mean_metro_dist",
                                               "mean_attr_index_norm_10")])
# no very high correlations (>0.85) anymore

