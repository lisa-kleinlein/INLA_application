library(INLA)
library(spdep)
library(sf)
library(rgdal)
library(tidyverse)
library(dplyr)
library(maptools)
library(ggpubr)
library(viridis)
library(tikzDevice)

## LONDON
ggplot(london_weekdays_grouped, aes(x = mean_realSum)) +
  geom_density() +
  labs(x = "mean price") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

ggplot(london_weekdays_grouped, aes(x = mean_log_realSum)) +
  geom_density() +
  labs(x = "log mean price") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

# check if normal distribution assumption for mean_log_realSum fits 
test_data <- london_weekdays_grouped %>%
  as.data.frame() %>%
  group_by(mean_room_shared_10, mean_room_private_10, mean_person_capacity,
           mean_host_is_superhost, quant_offers_2_4, quant_offers_more_than_4,
           mean_cleanliness_rating, mean_guest_satisfaction_overall_10, mean_bedrooms,
           mean_dist, mean_metro_dist, mean_attr_index_norm_10)
shapiro.test(test_data$mean_log_realSum)



shape_london <- maptools::readShapePoly("London-wards-2018/London-wards-2018_ESRI/London_Ward.shp")
plot(shape_london, border = "blue", axes = TRUE, las = 1)

london_weekdays_grouped <- london_weekdays_grouped %>% arrange(factor(GSS_CODE, levels = shape_london[shape_london$GSS_CODE %in% london_weekdays_grouped$GSS_CODE, ]$GSS_CODE))
london_weekdays_grouped$id <- 1:nrow(london_weekdays_grouped)

# spatial model
shape_nb_london <- spdep::poly2nb(shape_london[shape_london$GSS_CODE %in% london_weekdays_grouped$GSS_CODE, ])

nb2INLA("shape_inla_london.graph", shape_nb_london)
shape_adj_london <- paste(getwd(), "/shape_inla_london.graph", sep = "")

H <- inla.read.graph(filename = "shape_inla_london.graph")
image(inla.graph2matrix(H), xlab = "ward ID", ylab = "ward ID", main = "Adjacency matrix for wards of London")


# INLA with all covariates (just weekdays) log, gaussin
formula_weekdays_all <- mean_log_realSum ~ 1 +
  mean_room_shared_10 +
  mean_room_private_10 +
  mean_person_capacity +
  mean_bedrooms +
  mean_dist +
  mean_metro_dist +
  mean_host_is_superhost +
  quant_offers_2_4 +
  quant_offers_more_than_4 +
  mean_cleanliness_rating +
  mean_guest_satisfaction_overall_10 +
  mean_attr_index_norm_10 +
  f(id, model = "besag", graph = shape_adj_london)
model_weekdays_all <- inla(formula_weekdays_all,
                           family = "gaussian", data = london_weekdays_grouped,
                           control.fixed(prec.intercept = 0.001),
                           control.compute = list(dic = TRUE))
summary(model_weekdays_all)

effects_weekdays_all <- round(model_weekdays_all$summary.fixed, 3)
effects_weekdays_all$predictor <- rownames(effects_weekdays_all)
forest_data_weekdays_all <- data.frame("predictor" = rep(effects_weekdays_all$predictor, times = 3),
                                       "effects" = c(effects_weekdays_all$mean, effects_weekdays_all$`0.025quant`, effects_weekdays_all$`0.975quant`))
round(model_weekdays_all$summary.random$id, 3)
all(round(model_weekdays_all$summary.random$id, 3)$`0.975quant` > 0)
all(round(model_weekdays_all$summary.random$id, 3)$`0.025quant` < 0)
# the estimated spatial random effect for all wards is not significant

ggplot(forest_data_weekdays_all[forest_data_weekdays_all$predictor != "(Intercept)", ]) +
  geom_point(aes(x = effects,
                 y = factor(predictor,
                            levels = c("mean_room_shared_10", "mean_room_private_10", "mean_person_capacity",
                                       "mean_bedrooms", "mean_dist", "mean_metro_dist",
                                       "mean_host_is_superhost", "quant_offers_2_4", "quant_offers_more_than_4",
                                       "mean_cleanliness_rating", "mean_guest_satisfaction_overall_10", "mean_attr_index_norm_10")))) +
  geom_line(aes(x = effects,
                y = predictor)) +
  geom_vline(aes(xintercept = 0)) +
  labs(title = "Fixed effects with 95% confidence interval", y = "predictors") +
  theme_bw() +
  scale_y_discrete(limits = rev,
                   labels = c("mean_room_shared_10" = "room shared", "mean_room_private_10" = "room private",
                              "mean_person_capacity" = "person capacity", "mean_bedrooms" = "number of bedrooms",
                              "mean_dist" = "distance from city center", "mean_metro_dist" = "distance from metro",
                              "mean_host_is_superhost" = "superhost status", "quant_offers_2_4" = "2-4 offers",
                              "quant_offers_more_than_4" = "more than 4 offers", "mean_cleanliness_rating" = "cleanliness rating",
                              "mean_guest_satisfaction_overall_10" = "guest satisfication rating", "mean_attr_index_norm_10" = "location attraction index")) +
  theme(plot.title = element_text(size = 12))



# INLA with all covariates (just weekdays) non-log, gamma
formula_weekdays_all_gamma <- mean_realSum ~ 1 +
  mean_room_shared_10 +
  mean_room_private_10 +
  mean_person_capacity +
  mean_bedrooms +
  mean_dist +
  mean_metro_dist +
  mean_host_is_superhost +
  quant_offers_2_4 +
  quant_offers_more_than_4 +
  mean_cleanliness_rating +
  mean_guest_satisfaction_overall_10 +
  mean_attr_index_norm_10 +
  f(id, model = "besag", graph = shape_adj_london)
model_weekdays_all_gamma <- inla(formula_weekdays_all_gamma,
                           family = "gamma", data = london_weekdays_grouped,
                           control.fixed(prec.intercept = 0.001),
                           control.compute = list(dic = TRUE))
summary(model_weekdays_all_gamma)

effects_weekdays_all_gamma <- round(model_weekdays_all_gamma$summary.fixed, 3)
effects_weekdays_all_gamma$predictor <- rownames(effects_weekdays_all_gamma)
forest_data_weekdays_all_gamma <- data.frame("predictor" = rep(effects_weekdays_all_gamma$predictor, times = 3),
                                       "effects" = c(effects_weekdays_all_gamma$mean, effects_weekdays_all_gamma$`0.025quant`, effects_weekdays_all_gamma$`0.975quant`))
round(model_weekdays_all_gamma$summary.random$id, 3)
all(round(model_weekdays_all_gamma$summary.random$id, 3)$`0.975quant` > 0)
all(round(model_weekdays_all_gamma$summary.random$id, 3)$`0.025quant` < 0)
length(which(round(model_weekdays_all_gamma$summary.random$id, 3)$`0.025quant` > 0))
# 4 wards with significant estimated spatial random effect

ggplot(forest_data_weekdays_all_gamma[forest_data_weekdays_all_gamma$predictor != "(Intercept)", ]) +
  geom_point(aes(x = effects,
                 y = factor(predictor,
                            levels = c("mean_room_shared_10", "mean_room_private_10", "mean_person_capacity",
                                       "mean_bedrooms", "mean_dist", "mean_metro_dist",
                                       "mean_host_is_superhost", "quant_offers_2_4", "quant_offers_more_than_4",
                                       "mean_cleanliness_rating", "mean_guest_satisfaction_overall_10", "mean_attr_index_norm_10")))) +
  geom_line(aes(x = effects,
                y = predictor)) +
  geom_vline(aes(xintercept = 0)) +
  labs(title = "Fixed effects with 95% confidence interval", y = "predictors") +
  theme_bw() +
  scale_y_discrete(limits = rev,
                   labels = c("mean_room_shared_10" = "room shared", "mean_room_private_10" = "room private",
                              "mean_person_capacity" = "person capacity", "mean_bedrooms" = "number of bedrooms",
                              "mean_dist" = "distance from city center", "mean_metro_dist" = "distance from metro",
                              "mean_host_is_superhost" = "superhost status", "quant_offers_2_4" = "2-4 offers",
                              "quant_offers_more_than_4" = "more than 4 offers", "mean_cleanliness_rating" = "cleanliness rating",
                              "mean_guest_satisfaction_overall_10" = "guest satisfication rating", "mean_attr_index_norm_10" = "location attraction index")) +
  theme(plot.title = element_text(size = 12))





# INLA wo attr_ind and dist (just weekdays)
formula_weekdays_all_wo_attr_dist <- mean_log_realSum ~ 1 +
  mean_room_shared_10 +
  mean_room_private_10 +
  mean_person_capacity +
  mean_bedrooms +
  #mean_dist +
  mean_metro_dist +
  mean_host_is_superhost +
  quant_offers_2_4 +
  quant_offers_more_than_4 +
  mean_cleanliness_rating +
  mean_guest_satisfaction_overall_10 +
  #mean_attr_index_norm_10 +
  f(id, model = "besag", graph = shape_adj_london)
model_weekdays_all_wo_attr_dist <- inla(formula_weekdays_all_wo_attr_dist,
                           family = "gaussian", data = london_weekdays_grouped,
                           control.fixed(prec.intercept = 0.001),
                           control.compute = list(dic = TRUE))
summary(model_weekdays_all_wo_attr_dist)

effects_weekdays_all_wo_attr_dist <- round(model_weekdays_all_wo_attr_dist$summary.fixed, 3)
effects_weekdays_all_wo_attr_dist$predictor <- rownames(effects_weekdays_all_wo_attr_dist)
forest_data_weekdays_all_wo_attr_dist <- data.frame("predictor" = rep(effects_weekdays_all_wo_attr_dist$predictor, times = 3),
                                       "effects" = c(effects_weekdays_all_wo_attr_dist$mean, effects_weekdays_all_wo_attr_dist$`0.025quant`, effects_weekdays_all_wo_attr_dist$`0.975quant`))
round(model_weekdays_all_wo_attr_dist$summary.random$id, 3)
all(round(model_weekdays_all_wo_attr_dist$summary.random$id, 3)$`0.975quant` > 0)
length(which(round(model_weekdays_all_wo_attr_dist$summary.random$id, 3)$`0.975quant` < 0))
# 6 wards for which the estimated spatial random effect is significant negative (for alpha = 0.05)
all(round(model_weekdays_all_wo_attr_dist$summary.random$id, 3)$`0.025quant` < 0)
length(which(round(model_weekdays_all_wo_attr_dist$summary.random$id, 3)$`0.025quant` > 0))
# 13 wards for which the estimated spatial random effect is significant positive (for alpha = 0.05)
# -> 19 wards with significant spatial effect
# the estimated spatial random effect for all other wards is not significant

ggplot(forest_data_weekdays_all_wo_attr_dist[forest_data_weekdays_all_wo_attr_dist$predictor != "(Intercept)", ]) +
  geom_point(aes(x = effects,
                 y = factor(predictor,
                            levels = c("mean_room_shared_10", "mean_room_private_10", "mean_person_capacity",
                                       "mean_bedrooms", "mean_metro_dist",
                                       "mean_host_is_superhost", "quant_offers_2_4", "quant_offers_more_than_4",
                                       "mean_cleanliness_rating", "mean_guest_satisfaction_overall_10")))) +
  geom_line(aes(x = effects,
                y = predictor)) +
  geom_vline(aes(xintercept = 0)) +
  labs(title = "Fixed effects with 95% confidence interval", y = "predictors") +
  theme_bw() +
  scale_y_discrete(limits = rev,
                   labels = c("mean_room_shared_10" = "room shared", "mean_room_private_10" = "room private",
                              "mean_person_capacity" = "person capacity", "mean_bedrooms" = "number of bedrooms",
                              "mean_metro_dist" = "distance from metro", "mean_host_is_superhost" = "superhost status",
                              "quant_offers_2_4" = "2-4 offers", "quant_offers_more_than_4" = "more than 4 offers",
                              "mean_cleanliness_rating" = "cleanliness rating", "mean_guest_satisfaction_overall_10" = "guest satisfication rating")) +
  theme(plot.title = element_text(size = 12))








# Visualization
shape_london_w_polygon <- st_read("London-wards-2018/London-wards-2018_ESRI/London_Ward.shp")
shape_london_weekdays_w_polygon_available <- shape_london_w_polygon[shape_london_w_polygon$GSS_CODE %in% london_weekdays_grouped$GSS_CODE, ]
data_for_viz <- london_weekdays_grouped
data_for_viz$geometry_polygon <- shape_london_weekdays_w_polygon_available$geometry 

# wards with single observations
ggplot(data = data_for_viz) +
  geom_sf(aes(geometry = geometry_polygon)) +
  #geom_sf(aes(geometry = geometry), size = 0.7, shape = 1, col = "blue") +
  labs(title = "Central wards of London",
       caption = "Contains National Statistics data © Crown copyright and database right [2015]") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

# wards with number of observations
ggplot(data = data_for_viz) +
  geom_sf(aes(geometry = geometry_polygon, fill = n)) +
  labs(title = "Number of observations per ward",
       caption = "Contains National Statistics data © Crown copyright and database right [2015]") +
  scale_fill_continuous(name = "number of\nobservations") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        plot.caption = element_text(hjust = 0))

# wards with estimated random spatial effect (mean)
data_for_viz$estimated_random_effect <- model_weekdays_all$summary.random$id$mean
ggplot(data = data_for_viz) +
  geom_sf(aes(geometry = geometry_polygon, fill = estimated_random_effect)) +
  labs(title = "Estimated random spatial effects (mean) per ward",
       caption = "Contains National Statistics data © Crown copyright and database right [2015]") +
  scale_fill_viridis(name = "estimated\nrandom\neffect", limits = c(-0.05, 0.07)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

p1 <- ggplot(data = data_for_viz) +
  geom_sf(aes(geometry = geometry_polygon, fill = estimated_random_effect)) +
  labs(title = "mean") +
  scale_fill_viridis(name = "", limits = c(-0.05, 0.07)) +
  theme_bw() +
  theme(plot.title = element_text(size = 10)) +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

# wards with 0.025 quantile of estimated random spatial effect
data_for_viz$estimated_random_effect_0.025 <- model_weekdays_all$summary.random$id$`0.025quant`
ggplot(data = data_for_viz) +
  geom_sf(aes(geometry = geometry_polygon, fill = estimated_random_effect_0.025)) +
  labs(title = "0.025 quantile of estimated random spatial effects per ward",
       caption = "Contains National Statistics data © Crown copyright and database right [2015]") +
  scale_fill_viridis(name = "0.025 quantile", limits = c(-0.05, 0.07)) +
  theme_bw()  +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

p2 <- ggplot(data = data_for_viz) +
  geom_sf(aes(geometry = geometry_polygon, fill = estimated_random_effect_0.025)) +
  labs(title = "0.025 quantile") +
  scale_fill_viridis(name = "", limits = c(-0.05, 0.07)) +
  theme_bw() +
  theme(plot.title = element_text(size = 10))  +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

# wards with 0.975 quantile of estimated random spatial effect
data_for_viz$estimated_random_effect_0.975 <- model_weekdays_all$summary.random$id$`0.975quant`
ggplot(data = data_for_viz) +
  geom_sf(aes(geometry = geometry_polygon, fill = estimated_random_effect_0.975)) +
  labs(title = "0.975 quantile of estimated random spatial effects per ward",
       caption = "Contains National Statistics data © Crown copyright and database right [2015]") +
  scale_fill_viridis(name = "0.975 quantile", limits = c(-0.05, 0.07)) +
  theme_bw()  +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

p3 <- ggplot(data = data_for_viz) +
  geom_sf(aes(geometry = geometry_polygon, fill = estimated_random_effect_0.975)) +
  labs(title = "0.975 quantile") +
  scale_fill_viridis(name = "", limits = c(-0.05, 0.07)) +
  theme_bw() +
  theme(plot.title = element_text(size = 10)) +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())


plots_random_effects <- ggarrange(p1, NULL, p2, p3, common.legend = TRUE, legend = "right")
plots_random_effects
annotate_figure(plots_random_effects,
                top = text_grob("Spatial random effects", size = 12),
                bottom = text_grob("Contains National Statistics data © Crown copyright and database right [2015]", size = 10, hjust = 0.4, vjust = 0.3)
)




# for non-log mean price, gamma
# wards with estimated random spatial effect (mean)
data_for_viz$estimated_random_effect <- model_weekdays_all_gamma$summary.random$id$mean
ggplot(data = data_for_viz) +
  geom_sf(aes(geometry = geometry_polygon, fill = estimated_random_effect)) +
  labs(title = "Estimated random spatial effects (mean) per ward",
       caption = "Contains National Statistics data © Crown copyright and database right [2015]") +
  scale_fill_viridis(name = "estimated\nrandom\neffect", limits = c(-0.55, 1.05)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

p1 <- ggplot(data = data_for_viz) +
  geom_sf(aes(geometry = geometry_polygon, fill = estimated_random_effect)) +
  labs(title = "mean") +
  scale_fill_viridis(name = "", limits = c(-0.55, 1.05)) +
  theme_bw() +
  theme(plot.title = element_text(size = 10)) +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

# wards with 0.025 quantile of estimated random spatial effect
data_for_viz$estimated_random_effect_0.025 <- model_weekdays_all_gamma$summary.random$id$`0.025quant`
ggplot(data = data_for_viz) +
  geom_sf(aes(geometry = geometry_polygon, fill = estimated_random_effect_0.025)) +
  labs(title = "0.025 quantile of estimated random spatial effects per ward",
       caption = "Contains National Statistics data © Crown copyright and database right [2015]") +
  scale_fill_viridis(name = "0.025 quantile", limits = c(-0.55, 1.05)) +
  theme_bw()  +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

p2 <- ggplot(data = data_for_viz) +
  geom_sf(aes(geometry = geometry_polygon, fill = estimated_random_effect_0.025)) +
  labs(title = "0.025 quantile") +
  scale_fill_viridis(name = "", limits = c(-0.55, 1.05)) +
  theme_bw() +
  theme(plot.title = element_text(size = 10))  +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

# wards with 0.975 quantile of estimated random spatial effect
data_for_viz$estimated_random_effect_0.975 <- model_weekdays_all_gamma$summary.random$id$`0.975quant`
ggplot(data = data_for_viz) +
  geom_sf(aes(geometry = geometry_polygon, fill = estimated_random_effect_0.975)) +
  labs(title = "0.975 quantile of estimated random spatial effects per ward",
       caption = "Contains National Statistics data © Crown copyright and database right [2015]") +
  scale_fill_viridis(name = "0.975 quantile", limits = c(-0.55, 1.05)) +
  theme_bw()  +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

p3 <- ggplot(data = data_for_viz) +
  geom_sf(aes(geometry = geometry_polygon, fill = estimated_random_effect_0.975)) +
  labs(title = "0.975 quantile") +
  scale_fill_viridis(name = "", limits = c(-0.55, 1.05)) +
  theme_bw() +
  theme(plot.title = element_text(size = 10)) +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())


plots_random_effects <- ggarrange(p1, NULL, p2, p3, common.legend = TRUE, legend = "right")
plots_random_effects
annotate_figure(plots_random_effects,
                top = text_grob("Spatial random effects", size = 12),
                bottom = text_grob("Contains National Statistics data © Crown copyright and database right [2015]", size = 10, hjust = 0.4, vjust = 0.3)
)






# wo attr_ind
shape_london_w_polygon <- st_read("London-wards-2018/London-wards-2018_ESRI/London_Ward.shp")
shape_london_weekdays_w_polygon_available <- shape_london_w_polygon[shape_london_w_polygon$GSS_CODE %in% london_weekdays_grouped$GSS_CODE, ]
data_for_viz2 <- london_weekdays_grouped
data_for_viz2$geometry_polygon <- shape_london_weekdays_w_polygon_available$geometry 

# wards with estimated random spatial effect (mean)
data_for_viz2$estimated_random_effect <- model_weekdays_all_wo_attr_dist$summary.random$id$mean
ggplot(data = data_for_viz2) +
  geom_sf(aes(geometry = geometry_polygon, fill = estimated_random_effect)) +
  labs(title = "Estimated random spatial effects (mean) per ward",
       caption = "Contains National Statistics data © Crown copyright and database right [2015]") +
  scale_fill_viridis(name = "estimated\nrandom\neffect", limits = c(-0.6, 0.8)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

p1 <- ggplot(data = data_for_viz2) +
  geom_sf(aes(geometry = geometry_polygon, fill = estimated_random_effect)) +
  labs(title = "mean") +
  scale_fill_viridis(name = "", limits = c(-0.6, 0.8)) +
  theme_bw() +
  theme(plot.title = element_text(size = 10)) +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

# wards with 0.025 quantile of estimated random spatial effect
data_for_viz2$estimated_random_effect_0.025 <- model_weekdays_all_wo_attr_dist$summary.random$id$`0.025quant`
ggplot(data = data_for_viz2) +
  geom_sf(aes(geometry = geometry_polygon, fill = estimated_random_effect_0.025)) +
  labs(title = "0.025 quantile of estimated random spatial effects per ward",
       caption = "Contains National Statistics data © Crown copyright and database right [2015]") +
  scale_fill_viridis(name = "0.025 quantile", limits = c(-0.6, 0.8)) +
  theme_bw()  +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

p2 <- ggplot(data = data_for_viz2) +
  geom_sf(aes(geometry = geometry_polygon, fill = estimated_random_effect_0.025)) +
  labs(title = "0.025 quantile") +
  scale_fill_viridis(name = "", limits = c(-0.6, 0.8)) +
  theme_bw() +
  theme(plot.title = element_text(size = 10))  +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

# wards with 0.975 quantile of estimated random spatial effect
data_for_viz2$estimated_random_effect_0.975 <- model_weekdays_all_wo_attr_dist$summary.random$id$`0.975quant`
ggplot(data = data_for_viz2) +
  geom_sf(aes(geometry = geometry_polygon, fill = estimated_random_effect_0.975)) +
  labs(title = "0.975 quantile of estimated random spatial effects per ward",
       caption = "Contains National Statistics data © Crown copyright and database right [2015]") +
  scale_fill_viridis(name = "0.975 quantile", limits = c(-0.6, 0.8)) +
  theme_bw()  +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

p3 <- ggplot(data = data_for_viz2) +
  geom_sf(aes(geometry = geometry_polygon, fill = estimated_random_effect_0.975)) +
  labs(title = "0.975 quantile") +
  scale_fill_viridis(name = "", limits = c(-0.6, 0.8)) +
  theme_bw() +
  theme(plot.title = element_text(size = 10)) +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())


plots_random_effects <- ggarrange(p1, NULL, p2, p3, common.legend = TRUE, legend = "right")
plots_random_effects
annotate_figure(plots_random_effects,
                top = text_grob("Spatial random effects", size = 12),
                bottom = text_grob("Contains National Statistics data © Crown copyright and database right [2015]", size = 10, hjust = 0.45, vjust = 0.3)
)






