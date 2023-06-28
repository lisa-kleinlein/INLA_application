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
sf_london <- st_as_sf(london, coords = c("lng", "lat"), crs = 4326)

# Check the CRS of districts and sf_data
print(st_crs(shape_london))
print(st_crs(sf_london))

# Align the CRS of districts and sf_data
sf_london <- st_transform(sf_london, crs = st_crs(shape_london))

# Perform the spatial join
joined_data_london <- st_join(sf_london, shape_london)

london_grouped <- joined_data_london %>%
  group_by(GSS_CODE) %>%
  summarise(n = n(),
            mean_realSum = mean(realSum),
            mean_log_realSum = mean(log_realSum),
            quant_room_type_entireHomeApt = sum(room_type_entireHomeApt) / n,
            quant_room_type_privateRoom = sum(room_type_privateRoom) / n,
            quant_room_type_sharedRoom = sum(room_type_sharedRoom) / n,
            mean_room_shared = mean(room_shared),
            mean_room_private = mean(room_private),
            mean_person_capacity = mean(person_capacity),
            mean_host_is_superhost = mean(host_is_superhost),
            mean_host_is_superhost_weekend = mean(host_is_superhost[weekend_listing == 1]),
            mean_multi = mean(multi),
            mean_multi_weekend = mean(multi[weekend_listing == 1]),
            mean_biz = mean(biz),
            mean_biz_weekend = mean(biz[weekend_listing == 1]),
            mean_cleanliness_rating = mean(cleanliness_rating),
            mean_cleanliness_rating_weekend = mean(cleanliness_rating[weekend_listing == 1]),
            mean_guest_satisfaction_overall = mean(guest_satisfaction_overall),
            mean_guest_satisfaction_overall_weekend = mean(guest_satisfaction_overall[weekend_listing == 1]),
            mean_bedrooms = mean(bedrooms),
            mean_dist = mean(dist),
            mean_dist_weekend = mean(dist[weekend_listing == 1]),
            mean_metro_dist = mean(metro_dist),
            mean_metro_dist_weekend = mean(metro_dist[weekend_listing == 1]),
            mean_attr_index = mean(attr_index),
            mean_attr_index_weekend = mean(attr_index[weekend_listing == 1]),
            mean_attr_index_norm = mean(attr_index_norm),
            mean_rest_index = mean(rest_index),
            mean_rest_index_norm = mean(rest_index_norm),
            mean_weekend_listing = mean(weekend_listing),
            DISTRICT = getmode(DISTRICT),
            LAGSSCODE = getmode(LAGSSCODE),
            HECTARES = getmode(HECTARES),
            NONLD_AREA = getmode(NONLD_AREA),
            geometry = getmode(geometry))

cor(as.data.frame(london_grouped)[, c("quant_room_type_entireHomeApt",
                                      "quant_room_type_privateRoom",
                                      "quant_room_type_sharedRoom",
                                      "mean_room_shared",
                                      "mean_room_private",
                                      "mean_person_capacity",
                                      "mean_host_is_superhost",
                                      "mean_host_is_superhost_weekend",
                                      "mean_multi",
                                      "mean_multi_weekend",
                                      "mean_biz",
                                      "mean_biz_weekend",
                                      "mean_cleanliness_rating",
                                      "mean_cleanliness_rating_weekend",
                                      "mean_guest_satisfaction_overall",
                                      "mean_guest_satisfaction_overall_weekend",
                                      "mean_bedrooms",
                                      "mean_dist",
                                      "mean_dist_weekend",
                                      "mean_metro_dist",
                                      "mean_metro_dist_weekend",
                                      "mean_attr_index",
                                      "mean_attr_index_weekend",
                                      "mean_attr_index_norm",
                                      "mean_rest_index",
                                      "mean_rest_index_norm",
                                      "mean_weekend_listing")])





























# aggregate weekdays and weekeds separately
## London_weekdays
shape_london <- st_read("London-wards-2018/London-wards-2018_ESRI/London_Ward.shp")
sf_london_weekdays <- st_as_sf(london_weekdays, coords = c("lng", "lat"), crs = 4326)

# Check the CRS of districts and sf_data
print(st_crs(shape_london))
print(st_crs(sf_london_weekdays))

# Align the CRS of districts and sf_data
sf_london_weekdays <- st_transform(sf_london_weekdays, crs = st_crs(shape_london))

# Perform the spatial join
joined_data_london_weekdays <- st_join(sf_london_weekdays, shape_london)

london_weekdays_grouped <- joined_data_london_weekdays %>%
  group_by(GSS_CODE) %>%
  summarise(n_weekdays = n(),
            mean_realSum_weekdays = mean(realSum),
            mean_log_realSum_weekdays = mean(log_realSum),
            quant_room_type_entireHomeApt_weekdays = sum(room_type_entireHomeApt) / n_weekdays,
            quant_room_type_privateRoom_weekdays = sum(room_type_privateRoom) / n_weekdays,
            quant_room_type_sharedRoom_weekdays = sum(room_type_sharedRoom) / n_weekdays,
            mean_room_shared_weekdays = mean(room_shared),
            mean_room_private_weekdays = mean(room_private),
            mean_person_capacity_weekdays = mean(person_capacity),
            mean_host_is_superhost_weekdays = mean(host_is_superhost),
            mean_multi_weekdays = mean(multi),
            mean_biz_weekdays = mean(biz),
            mean_cleanliness_rating_weekdays = mean(cleanliness_rating),
            mean_guest_satisfaction_overall_weekdays = mean(guest_satisfaction_overall),
            mean_bedrooms_weekdays = mean(bedrooms),
            mean_dist_weekdays = mean(dist),
            mean_metro_dist_weekdays = mean(metro_dist),
            mean_attr_index_weekdays = mean(attr_index),
            mean_attr_index_norm_weekdays = mean(attr_index_norm),
            mean_rest_index_weekdays = mean(rest_index),
            mean_rest_index_norm_weekdays = mean(rest_index_norm),
            DISTRICT = getmode(DISTRICT),
            LAGSSCODE = getmode(LAGSSCODE),
            HECTARES = getmode(HECTARES),
            NONLD_AREA = getmode(NONLD_AREA),
            geometry_weekdays = getmode(geometry))




## London_weekends
shape_london <- st_read("London-wards-2018/London-wards-2018_ESRI/London_Ward.shp")
sf_london_weekends <- st_as_sf(london_weekends, coords = c("lng", "lat"), crs = 4326)

# Check the CRS of districts and sf_data
print(st_crs(shape_london))
print(st_crs(sf_london_weekends))

# Align the CRS of districts and sf_data
sf_london_weekends <- st_transform(sf_london_weekends, crs = st_crs(shape_london))

# Perform the spatial join
joined_data_london_weekends <- st_join(sf_london_weekends, shape_london)

london_weekends_grouped <- joined_data_london_weekends %>%
  group_by(GSS_CODE) %>%
  summarise(n_weekends = n(),
            mean_realSum_weekends = mean(realSum),
            mean_log_realSum_weekends = mean(log_realSum),
            quant_room_type_entireHomeApt_weekends = sum(room_type_entireHomeApt) / n_weekends,
            quant_room_type_privateRoom_weekends = sum(room_type_privateRoom) / n_weekends,
            quant_room_type_sharedRoom_weekends = sum(room_type_sharedRoom) / n_weekends,
            mean_room_shared_weekends = mean(room_shared),
            mean_room_private_weekends = mean(room_private),
            mean_person_capacity_weekends = mean(person_capacity),
            mean_host_is_superhost_weekends = mean(host_is_superhost),
            mean_multi_weekends = mean(multi),
            mean_biz_weekends = mean(biz),
            mean_cleanliness_rating_weekends = mean(cleanliness_rating),
            mean_guest_satisfaction_overall_weekends = mean(guest_satisfaction_overall),
            mean_bedrooms_weekends = mean(bedrooms),
            mean_dist_weekends = mean(dist),
            mean_metro_dist_weekends = mean(metro_dist),
            mean_attr_index_weekends = mean(attr_index),
            mean_attr_index_norm_weekends = mean(attr_index_norm),
            mean_rest_index_weekends = mean(rest_index),
            mean_rest_index_norm_weekends = mean(rest_index_norm),
            geometry_weekends = getmode(geometry))

london_grouped <- st_join(london_weekdays_grouped, london_weekends_grouped, by = "GSS_CODE", left = FALSE)
london_grouped <- london_grouped %>%
  mutate(mean_realSum = mean_realSum_weekdays * (n_weekdays / (n_weekdays + n_weekends)) + mean_realSum_weekends * (n_weekends / (n_weekdays + n_weekends)),
         mean_log_realSum = mean_log_realSum_weekdays * (n_weekdays / (n_weekdays + n_weekends)) + mean_log_realSum_weekends * (n_weekends / (n_weekdays + n_weekends)))
