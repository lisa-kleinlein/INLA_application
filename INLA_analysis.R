library(INLA)
library(spdep)
library(sf)
library(rgdal)
library(tidyverse)
library(dplyr)
library(maptools)
library(ggpubr)
library(viridis)

## LONDON
ggplot(london_grouped, aes(y = mean_log_realSum)) +
  geom_density() +
  coord_flip() +
  theme_bw()


shape_london <- maptools::readShapePoly("London-wards-2018/London-wards-2018_ESRI/London_Ward.shp")
plot(shape_london, border = "blue", axes = TRUE, las = 1)

london_grouped <- london_grouped %>% arrange(factor(GSS_CODE, levels = shape_london[shape_london$GSS_CODE %in% london_grouped$GSS_CODE, ]$GSS_CODE))
london_grouped$id <- 1:nrow(london_grouped)

# spatial model
shape_nb_london <- spdep::poly2nb(shape_london[shape_london$GSS_CODE %in% london_grouped$GSS_CODE, ])

nb2INLA("shape_inla_london.graph", shape_nb_london)
shape_adj_london <- paste(getwd(), "/shape_inla_london.graph", sep = "")

H <- inla.read.graph(filename = "shape_inla_london.graph")
image(inla.graph2matrix(H), xlab = "", ylab = "")


# approach with several covariates (without weekend-interactions)
formula_inla_london_wo_weekend_interac <- mean_log_realSum ~ 1 +
  # quant_room_type_entireHomeApt +
  # quant_room_type_privateRoom +
  # quant_room_type_sharedRoom +
  mean_room_shared +
  mean_room_private +
  mean_person_capacity +
  mean_multi +
  mean_biz +
  mean_dist +
  mean_metro_dist +
  mean_host_is_superhost +
  mean_cleanliness_rating +
  mean_guest_satisfaction_overall +
  mean_attr_index +
  mean_weekend_listing +
  f(id, model = "besag", graph = shape_adj_london)
model_inla_london_wo_weekend_interac <- inla(formula_inla_london_wo_weekend_interac,
                                             family = "gaussian", data = london_grouped,
                                             control.compute = list(dic = TRUE))

summary(model_inla_london_wo_weekend_interac)

effects_wo_weekend_interac <- round(model_inla_london_wo_weekend_interac$summary.fixed, 3)
effects_wo_weekend_interac$predictor <- rownames(effects_wo_weekend_interac)
forest_data_wo_weekend_interac <- data.frame("predictor" = rep(effects_wo_weekend_interac$predictor, times = 3),
                          "effects" = c(effects_wo_weekend_interac$mean, effects_wo_weekend_interac$`0.025quant`, effects_wo_weekend_interac$`0.975quant`))
round(model_inla_london_wo_weekend_interac$summary.random$id, 3)

ggplot(forest_data_wo_weekend_interac[forest_data_wo_weekend_interac$predictor != "(Intercept)", ]) +
  geom_point(aes(x = effects, y = predictor)) +
  geom_line(aes(x = effects, y = predictor)) +
  geom_vline(aes(xintercept = 0)) +
  labs(title = "Without weekend interactions, all") +
  theme_bw() +
  xlim(-1.5, 1.5)


# approach with several covariates (without weekend-interactions, better dic)
# excluded: mean_room_private, mean_attr_index, mean_dist, mean_metro_dist, mean_multi
formula_inla_london_wo_weekend_interac_dic <- mean_log_realSum ~ 1 +
  # quant_room_type_entireHomeApt +
  # quant_room_type_privateRoom +
  # quant_room_type_sharedRoom +
  mean_room_shared +
  # mean_room_private +
  mean_person_capacity +
  # mean_multi +
  mean_biz +
  # mean_dist +
  # mean_metro_dist +
  mean_host_is_superhost +
  mean_cleanliness_rating +
  mean_guest_satisfaction_overall +
  # mean_attr_index +
  mean_weekend_listing +
  f(id, model = "besag", graph = shape_adj_london)
model_inla_london_wo_weekend_interac_dic <- inla(formula_inla_london_wo_weekend_interac_dic,
                                             family = "gaussian", data = london_grouped,
                                             control.compute = list(dic = TRUE))

summary(model_inla_london_wo_weekend_interac_dic)

effects_wo_weekend_interac_dic <- round(model_inla_london_wo_weekend_interac_dic$summary.fixed, 3)
effects_wo_weekend_interac_dic$predictor <- rownames(effects_wo_weekend_interac_dic)
forest_data_wo_weekend_interac_dic <- data.frame("predictor" = rep(effects_wo_weekend_interac_dic$predictor, times = 3),
                                             "effects" = c(effects_wo_weekend_interac_dic$mean,
                                                           effects_wo_weekend_interac_dic$`0.025quant`,
                                                           effects_wo_weekend_interac_dic$`0.975quant`))
round(model_inla_london_wo_weekend_interac_dic$summary.random$id, 3)

ggplot(forest_data_wo_weekend_interac_dic[forest_data_wo_weekend_interac_dic$predictor != "(Intercept)", ]) +
  geom_point(aes(x = effects, y = predictor)) +
  geom_line(aes(x = effects, y = predictor)) +
  geom_vline(aes(xintercept = 0)) +
  labs(title = "Without weekend interactions, selection with better DIC") +
  theme_bw() +
  xlim(-1.75, 1.75)


# approach with several covariates (without weekend-interactions, some)
formula_inla_london_wo_weekend_interac_some <- mean_log_realSum ~ 1 +
  # quant_room_type_entireHomeApt +
  # quant_room_type_privateRoom +
  # quant_room_type_sharedRoom +
  mean_room_shared +
  mean_room_private +
  mean_person_capacity +
  mean_multi +
  mean_dist +
  mean_metro_dist +
  mean_weekend_listing +
  f(id, model = "besag", graph = shape_adj_london)
model_inla_london_wo_weekend_interac_some <- inla(formula_inla_london_wo_weekend_interac_some,
                                                  family = "gaussian", data = london_grouped,
                                                  control.compute = list(dic = TRUE))

summary(model_inla_london_wo_weekend_interac_some)

effects_wo_weekend_interac_some <- round(model_inla_london_wo_weekend_interac_some$summary.fixed, 3)
effects_wo_weekend_interac_some$predictor <- rownames(effects_wo_weekend_interac_some)
forest_data_wo_weekend_interac_some <- data.frame("predictor" = rep(effects_wo_weekend_interac_some$predictor, times = 3),
                                                  "effects" = c(effects_wo_weekend_interac_some$mean,
                                                                effects_wo_weekend_interac_some$`0.025quant`,
                                                                effects_wo_weekend_interac_some$`0.975quant`))
round(model_inla_london_wo_weekend_interac_some$summary.random$id, 3)

ggplot(forest_data_wo_weekend_interac_some[forest_data_wo_weekend_interac_some$predictor != "(Intercept)", ]) +
  geom_point(aes(x = effects, y = predictor)) +
  geom_line(aes(x = effects, y = predictor)) +
  geom_vline(aes(xintercept = 0)) +
  labs(title = "Without weekend interactions, selection, just chosen some") +
  theme_bw() +
  xlim(-1.5, 1.5)





# approach with several covariates (with weekend-interactions)
formula_inla_london_w_weekend_interac <- mean_log_realSum ~ 1 +
  # quant_room_type_entireHomeApt +
  # quant_room_type_privateRoom +
  # quant_room_type_sharedRoom +
  mean_room_shared +
  mean_room_private +
  mean_person_capacity +
  mean_multi +
  mean_multi_weekend +
  mean_biz +
  mean_biz_weekend +
  mean_dist +
  mean_dist_weekend +
  mean_metro_dist +
  mean_metro_dist_weekend +
  mean_host_is_superhost +
  mean_host_is_superhost_weekend +
  mean_cleanliness_rating +
  mean_cleanliness_rating_weekend +
  mean_guest_satisfaction_overall +
  mean_guest_satisfaction_overall_weekend +
  mean_attr_index +
  mean_attr_index_weekend +
  mean_weekend_listing +
  f(id, model = "besag", graph = shape_adj_london)
model_inla_london_w_weekend_interac <- inla(formula_inla_london_w_weekend_interac,
                                            family = "gaussian", data = london_grouped,
                                            control.compute = list(dic = TRUE))

summary(model_inla_london_w_weekend_interac)

effects_w_weekend_interac <- round(model_inla_london_w_weekend_interac$summary.fixed, 3)
effects_w_weekend_interac$predictor <- rownames(effects_w_weekend_interac)
forest_data_w_weekend_interac <- data.frame("predictor" = rep(effects_w_weekend_interac$predictor, times = 3),
                                            "effects" = c(effects_w_weekend_interac$mean, effects_w_weekend_interac$`0.025quant`, effects_w_weekend_interac$`0.975quant`))
round(model_inla_london_w_weekend_interac$summary.random$id, 3)

ggplot(forest_data_w_weekend_interac[forest_data_w_weekend_interac$predictor != "(Intercept)", ]) +
  geom_point(aes(x = effects, y = predictor)) +
  geom_line(aes(x = effects, y = predictor)) +
  geom_vline(aes(xintercept = 0)) +
  labs(title = "With weekend interactions, all") +
  theme_bw() +
  xlim(-1.5, 1.5)


# approach with several covariates (with weekend-interactions, some)
formula_inla_london_w_weekend_interac_some <- mean_log_realSum ~ 1 +
  # quant_room_type_entireHomeApt +
  # quant_room_type_privateRoom +
  # quant_room_type_sharedRoom +
  mean_room_shared +
  mean_room_private +
  mean_person_capacity +
  mean_multi +
  mean_multi_weekend +
  mean_dist +
  mean_dist_weekend +
  mean_metro_dist +
  mean_metro_dist_weekend +
  mean_weekend_listing +
  f(id, model = "besag", graph = shape_adj_london)
model_inla_london_w_weekend_interac_some <- inla(formula_inla_london_w_weekend_interac_some,
                                                 family = "gaussian", data = london_grouped,
                                                 control.compute = list(dic = TRUE))

summary(model_inla_london_w_weekend_interac_some)

effects_w_weekend_interac_some <- round(model_inla_london_w_weekend_interac_some$summary.fixed, 3)
effects_w_weekend_interac_some$predictor <- rownames(effects_w_weekend_interac_some)
forest_data_w_weekend_interac_some <- data.frame("predictor" = rep(effects_w_weekend_interac_some$predictor, times = 3),
                                                  "effects" = c(effects_w_weekend_interac_some$mean,
                                                                effects_w_weekend_interac_some$`0.025quant`,
                                                                effects_w_weekend_interac_some$`0.975quant`))
round(model_inla_london_w_weekend_interac_some$summary.random$id, 3)

ggplot(forest_data_w_weekend_interac_some[forest_data_w_weekend_interac_some$predictor != "(Intercept)", ]) +
  geom_point(aes(x = effects, y = predictor)) +
  geom_line(aes(x = effects, y = predictor)) +
  geom_vline(aes(xintercept = 0)) +
  labs(title = "With weekend interactions, selection, just chosen some") +
  theme_bw() +
  xlim(-1.5, 1.5)



summary(model_inla_london_wo_weekend_interac)
summary(model_inla_london_wo_weekend_interac_some)
summary(model_inla_london_w_weekend_interac)
summary(model_inla_london_w_weekend_interac_some)



















# # aggregate weekdays and weekeds separately
# shape_london <- maptools::readShapePoly("London-wards-2018/London-wards-2018_ESRI/London_Ward.shp")
# plot(shape_london, border = "blue", axes = TRUE, las = 1)
# 
# london_grouped <- london_grouped %>% arrange(factor(GSS_CODE.x, levels = shape_london[shape_london$GSS_CODE %in% london_grouped$GSS_CODE.x, ]$GSS_CODE))
# london_grouped$id <- 1:nrow(london_grouped)
# 
# # spatial model
# shape_nb_london <- spdep::poly2nb(shape_london[shape_london$GSS_CODE %in% london_grouped$GSS_CODE.x, ])
# 
# nb2INLA("shape_inla_london.graph", shape_nb_london)
# shape_adj_london <- paste(getwd(), "/shape_inla_london.graph", sep = "")
# 
# H <- inla.read.graph(filename = "shape_inla_london.graph")
# image(inla.graph2matrix(H), xlab = "", ylab = "")
# 
# 
# # very simple approach several covariates
# formula_inla_london <- mean_log_realSum ~ 1 +
#   #quant_room_type_entireHomeApt_weekdays +
#   #quant_room_type_entireHomeApt_weekends +
#   quant_room_type_privateRoom_weekdays +
#   quant_room_type_privateRoom_weekends +
#   #quant_room_type_sharedRoom_weekdays +
#   #quant_room_type_sharedRoom_weekends +
#   # mean_room_shared +
#   # # mean_room_private +
#   #mean_person_capacity_weekdays +
#   #mean_person_capacity_weekends +
#   #mean_multi_weekdays +
#   #mean_multi_weekends +
#   #mean_biz_weekdays +
#   #mean_biz_weekends +
#   #mean_dist_weekdays +
#   #mean_dist_weekends +
#   #mean_metro_dist_weekdays +
#   #mean_metro_dist_weekends +
#   #mean_host_is_superhost_weekdays +
#   #mean_host_is_superhost_weekends +
#   #mean_cleanliness_rating_weekdays +
#   #mean_cleanliness_rating_weekends +
#   #mean_guest_satisfaction_overall_weekdays +
#   #mean_guest_satisfaction_overall_weekends +
#   #mean_attr_index_weekdays +
#   #mean_attr_index_weekends +
#   f(id, model = "besag", graph = shape_adj_london)
# model_inla_london <- inla(formula_inla_london, family = "gaussian", data = london_grouped)
# 
# summary(model_inla_london)
# 





















 
# Visualization
shape_london_w_polygon <- st_read("London-wards-2018/London-wards-2018_ESRI/London_Ward.shp")
shape_london_w_polygon_available <- shape_london_w_polygon[shape_london_w_polygon$GSS_CODE %in% london_grouped$GSS_CODE, ]
data_for_viz <- london_grouped
data_for_viz$geometry_polygon <- shape_london_w_polygon_available$geometry 

# wards with single observations
ggplot(data = data_for_viz) +
  geom_sf(aes(geometry = geometry_polygon)) +
  geom_sf(aes(geometry = geometry), size = 0.7, shape = 1, col = "blue") +
  labs(title = "Central wards of london", x = "longitudinal", y = "latitudinal") +
  theme_bw()

# wards with number of observations
ggplot(data = data_for_viz) +
  geom_sf(aes(geometry = geometry_polygon, fill = n)) +
  labs(title = "Number of observations per ward", x = "longitudinal", y = "latitudinal") +
  scale_fill_continuous(name = "number of\nobservations") +
  theme_bw()

# wards with estimated random spatial effect (mean)
data_for_viz$estimated_random_effect <- model_inla_london_wo_weekend_interac_dic$summary.random$id$mean
ggplot(data = data_for_viz) +
  geom_sf(aes(geometry = geometry_polygon, fill = estimated_random_effect)) +
  labs(title = "Estimated random spatial effects (mean) per ward", x = "longitudinal", y = "latitudinal") +
  scale_fill_viridis(name = "estimated\nrandom\neffect", limits = c(-1.3, 1.3)) +
  theme_bw()

p1 <- ggplot(data = data_for_viz) +
  geom_sf(aes(geometry = geometry_polygon, fill = estimated_random_effect)) +
  labs(title = "mean") +
  scale_fill_viridis(name = "", limits = c(-1.3, 1.3)) +
  theme_bw() +
  theme(plot.title = element_text(size = 10))

# wards with 0.025 quantile of estimated random spatial effect
data_for_viz$estimated_random_effect_0.025 <- model_inla_london_wo_weekend_interac_dic$summary.random$id$`0.025quant`
ggplot(data = data_for_viz) +
  geom_sf(aes(geometry = geometry_polygon, fill = estimated_random_effect_0.025)) +
  labs(title = "0.025 quantile of estimated random spatial effects per ward", x = "longitudinal", y = "latitudinal") +
  scale_fill_viridis(name = "0.025 quantile", limits = c(-1.3, 1.3)) +
  theme_bw()

p2 <- ggplot(data = data_for_viz) +
  geom_sf(aes(geometry = geometry_polygon, fill = estimated_random_effect_0.025)) +
  labs(title = "0.025 quantile") +
  scale_fill_viridis(name = "", limits = c(-1.3, 1.3)) +
  theme_bw() +
  theme(plot.title = element_text(size = 10))

# wards with 0.975 quantile of estimated random spatial effect
data_for_viz$estimated_random_effect_0.975 <- model_inla_london_wo_weekend_interac_dic$summary.random$id$`0.975quant`
ggplot(data = data_for_viz) +
  geom_sf(aes(geometry = geometry_polygon, fill = estimated_random_effect_0.975)) +
  labs(title = "0.975 quantile of estimated random spatial effects per ward", x = "longitudinal", y = "latitudinal") +
  scale_fill_viridis(name = "0.975 quantile", limits = c(-1.3, 1.3)) +
  theme_bw()

p3 <- ggplot(data = data_for_viz) +
  geom_sf(aes(geometry = geometry_polygon, fill = estimated_random_effect_0.975)) +
  labs(title = "0.975 quantile") +
  scale_fill_viridis(name = "", limits = c(-1.3, 1.3)) +
  theme_bw() +
  theme(plot.title = element_text(size = 10))


plots_random_effects <- ggarrange(p1, NULL, p2, p3, common.legend = TRUE, legend = "right")
plots_random_effects
annotate_figure(plots_random_effects,
                top = text_grob("Estimated spatial random effects"),
                left = text_grob("latitudinal", rot = 90),
                bottom = text_grob("longitudinal")
)
