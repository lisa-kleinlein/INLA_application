library(INLA)
library(spdep)
library(sf)
library(tidyverse)
library(dplyr)
library(maptools)


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


# INLA for non-log mean price with all covariates
formula_weekdays_all_gamma <- mean_realSum ~ 1 +
  mean_room_shared_10 +
  mean_room_private_10 +
  mean_person_capacity +
  mean_bedrooms +
  mean_dist +
  mean_metro_dist +
  mean_host_is_superhost_10 +
  quant_offers_2_4_10  +
  quant_offers_more_than_4_10 +
  mean_cleanliness_rating +
  mean_guest_satisfaction_overall_10 +
  mean_attr_index_norm_10 +
  f(id, model = "besag", graph = shape_adj_london)
model_weekdays_all_gamma <- inla(formula_weekdays_all_gamma,
                                 family = "gamma", data = london_weekdays_grouped,
                                 control.fixed(prec.intercept = 0.001),
                                 control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE))
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
  geom_point(aes(x = exp(effects),
                 y = factor(predictor,
                            levels = c("mean_room_shared_10", "mean_room_private_10", "mean_person_capacity",
                                       "mean_bedrooms", "mean_dist", "mean_metro_dist",
                                       "mean_host_is_superhost_10", "quant_offers_2_4_10", "quant_offers_more_than_4_10",
                                       "mean_cleanliness_rating", "mean_guest_satisfaction_overall_10", "mean_attr_index_norm_10")))) +
  geom_line(aes(x = exp(effects),
                y = predictor)) +
  geom_vline(aes(xintercept = 1)) +
  labs(title = "Multiplicative fixed effects with 95% confidence interval", x = "exp(coefficient)", y = "predictors") +
  theme_bw() +
  scale_y_discrete(limits = rev,
                   labels = c("mean_room_shared_10" = "room shared", "mean_room_private_10" = "room private",
                              "mean_person_capacity" = "person capacity", "mean_bedrooms" = "number of bedrooms",
                              "mean_dist" = "distance from city center", "mean_metro_dist" = "distance from metro",
                              "mean_host_is_superhost_10" = "superhost status", "quant_offers_2_4_10" = "2-4 offers",
                              "quant_offers_more_than_4_10" = "more than 4 offers", "mean_cleanliness_rating" = "cleanliness rating",
                              "mean_guest_satisfaction_overall_10" = "guest satisfication rating", "mean_attr_index_norm_10" = "location attraction index")) +
  theme(plot.title = element_text(size = 12))



table_effects_all_gamma <- effects_weekdays_all_gamma[rownames(effects_weekdays_all_gamma) != "(Intercept)", c("mean", "0.025quant", "0.975quant")]
rownames(table_effects_all_gamma) <- c("room shared", "room private", "person capacity", "number of bedrooms", "distance from city center",
                                       "distance from metro", "superhost status", "2-4 offers", "more than 4 offers",
                                       "cleanliness rating", "guest satisfication rating", "location attraction index")

table_effects_all_gamma <- round(exp(table_effects_all_gamma), 3)


# INLA with all covariates (just weekdays) log, gaussian
formula_weekdays_all <- mean_log_realSum ~ 1 +
  mean_room_shared_10 +
  mean_room_private_10 +
  mean_person_capacity +
  mean_bedrooms +
  mean_dist +
  mean_metro_dist +
  mean_host_is_superhost_10 +
  quant_offers_2_4_10  +
  quant_offers_more_than_4_10 +
  mean_cleanliness_rating +
  mean_guest_satisfaction_overall_10 +
  mean_attr_index_norm_10 +
  f(id, model = "besag", graph = shape_adj_london)
model_weekdays_all <- inla(formula_weekdays_all,
                           family = "gaussian", data = london_weekdays_grouped,
                           control.fixed(prec.intercept = 0.001),
                           control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE))
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
                                       "mean_host_is_superhost_10", "quant_offers_2_4_10", "quant_offers_more_than_4_10",
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
                              "mean_host_is_superhost_10" = "superhost status", "quant_offers_2_4_10" = "2-4 offers",
                              "quant_offers_more_than_4_10" = "more than 4 offers", "mean_cleanliness_rating" = "cleanliness rating",
                              "mean_guest_satisfaction_overall_10" = "guest satisfication rating", "mean_attr_index_norm_10" = "location attraction index")) +
  theme(plot.title = element_text(size = 12))

table_effects_all <- effects_weekdays_all[rownames(effects_weekdays_all) != "(Intercept)", c("mean", "0.025quant", "0.975quant")]
rownames(table_effects_all) <- c("room shared", "room private", "person capacity", "number of bedrooms", "distance from city center",
                                 "distance from metro", "superhost status", "2-4 offers", "more than 4 offers",
                                 "cleanliness rating", "guest satisfication rating", "location attraction index")



# INLA log mean price and wo attr_ind and dist
formula_weekdays_all_wo_attr_dist <- mean_log_realSum ~ 1 +
  mean_room_shared_10 +
  mean_room_private_10 +
  mean_person_capacity +
  mean_bedrooms +
  #mean_dist +
  mean_metro_dist +
  mean_host_is_superhost_10 +
  quant_offers_2_4_10 +
  quant_offers_more_than_4_10 +
  mean_cleanliness_rating +
  mean_guest_satisfaction_overall_10 +
  #mean_attr_index_norm_10 +
  f(id, model = "besag", graph = shape_adj_london)
model_weekdays_all_wo_attr_dist <- inla(formula_weekdays_all_wo_attr_dist,
                                        family = "gaussian", data = london_weekdays_grouped,
                                        control.fixed(prec.intercept = 0.001),
                                        control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE))
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
                                       "mean_host_is_superhost_10", "quant_offers_2_4_10", "quant_offers_more_than_4_10",
                                       "mean_cleanliness_rating", "mean_guest_satisfaction_overall_10")))) +
  geom_line(aes(x = effects,
                y = predictor)) +
  geom_vline(aes(xintercept = 0)) +
  labs(title = "Fixed effects with 95% confidence interval", y = "predictors") +
  theme_bw() +
  scale_y_discrete(limits = rev,
                   labels = c("mean_room_shared_10" = "room shared", "mean_room_private_10" = "room private",
                              "mean_person_capacity" = "person capacity", "mean_bedrooms" = "number of bedrooms",
                              "mean_metro_dist" = "distance from metro", "mean_host_is_superhost_10" = "superhost status",
                              "quant_offers_2_4_10" = "2-4 offers", "quant_offers_more_than_4_10" = "more than 4 offers",
                              "mean_cleanliness_rating" = "cleanliness rating", "mean_guest_satisfaction_overall_10" = "guest satisfication rating")) +
  theme(plot.title = element_text(size = 12))


table_effects_selected <- effects_weekdays_all_wo_attr_dist[rownames(effects_weekdays_all_wo_attr_dist) != "(Intercept)", c("mean", "0.025quant", "0.975quant")]
rownames(table_effects_selected) <- c("room shared", "room private", "person capacity", "number of bedrooms",
                                 "distance from metro", "superhost status", "2-4 offers", "more than 4 offers",
                                 "cleanliness rating", "guest satisfication rating")




# Comparing models
# DIC
dic <- c(summary(model_weekdays_all_gamma)$dic$dic, summary(model_weekdays_all)$dic$dic, summary(model_weekdays_all_wo_attr_dist)$dic$dic)
names(dic) <- c("gamma_all", "gaussian_all", "gaussian_selected")
dic

# WAIC
waic <- c(summary(model_weekdays_all_gamma)$waic$waic, summary(model_weekdays_all)$waic$waic, summary(model_weekdays_all_wo_attr_dist)$waic$waic)
names(waic) <- c("gamma_all", "gaussian_all", "gaussian_selected")
waic
