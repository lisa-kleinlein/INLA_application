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
ggplot(london_weekdays_grouped, aes(y = mean_log_realSum)) +
  geom_density() +
  coord_flip() +
  theme_bw()


shape_london <- maptools::readShapePoly("London-wards-2018/London-wards-2018_ESRI/London_Ward.shp")
plot(shape_london, border = "blue", axes = TRUE, las = 1)

london_weekdays_grouped <- london_weekdays_grouped %>% arrange(factor(GSS_CODE, levels = shape_london[shape_london$GSS_CODE %in% london_weekdays_grouped$GSS_CODE, ]$GSS_CODE))
london_weekdays_grouped$id <- 1:nrow(london_weekdays_grouped)

# spatial model
shape_nb_london <- spdep::poly2nb(shape_london[shape_london$GSS_CODE %in% london_weekdays_grouped$GSS_CODE, ])

nb2INLA("shape_inla_london.graph", shape_nb_london)
shape_adj_london <- paste(getwd(), "/shape_inla_london.graph", sep = "")

H <- inla.read.graph(filename = "shape_inla_london.graph")
image(inla.graph2matrix(H), xlab = "", ylab = "")


# approach with all covariates (just weekdays)
formula_weekdays_all <- mean_log_realSum ~ 1 +
  mean_room_shared +
  mean_room_private +
  mean_person_capacity +
  mean_bedrooms +
  mean_dist +
  mean_metro_dist +
  mean_host_is_superhost +
  quant_offers_2_4 +
  quant_offers_more_than_4 +
  mean_cleanliness_rating +
  mean_guest_satisfaction_overall +
  mean_attr_index +
  f(id, model = "besag", graph = shape_adj_london)
model_weekdays_all <- inla(formula_weekdays_all,
                           family = "gaussian", data = london_weekdays_grouped,
                           control.compute = list(dic = TRUE))

summary(model_weekdays_all)

effects_weekdays_all <- round(model_weekdays_all$summary.fixed, 3)
effects_weekdays_all$predictor <- rownames(effects_weekdays_all)
forest_data_weekdays_all <- data.frame("predictor" = rep(effects_weekdays_all$predictor, times = 3),
                                       "effects" = c(effects_weekdays_all$mean, effects_weekdays_all$`0.025quant`, effects_weekdays_all$`0.975quant`))
round(model_weekdays_all$summary.random$id, 3)

ggplot(forest_data_weekdays_all[forest_data_weekdays_all$predictor != "(Intercept)", ]) +
  geom_point(aes(x = effects,
                 y = factor(predictor,
                            levels = c("mean_room_shared", "mean_room_private", "mean_person_capacity",
                                       "mean_bedrooms", "mean_dist", "mean_metro_dist",
                                       "mean_host_is_superhost", "quant_offers_2_4", "quant_offers_more_than_4",
                                       "mean_cleanliness_rating", "mean_guest_satisfaction_overall", "mean_attr_index")))) +
  geom_line(aes(x = effects,
                y = predictor)) +
  geom_vline(aes(xintercept = 0)) +
  labs(title = "Only weekdays, all covariates", y = "predictors") +
  theme_bw() +
  xlim(-1.5, 1.5) +
  scale_y_discrete(limits=rev)
