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

# distributions
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

# Visualization general
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








# Visualization for: log mean price, all covariates
data_for_viz <- london_weekdays_grouped
data_for_viz$geometry_polygon <- shape_london_weekdays_w_polygon_available$geometry 

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




# Visualization for: non-log mean price, all covariates
# wards with estimated random spatial effect (mean)
shape_london_w_polygon <- st_read("London-wards-2018/London-wards-2018_ESRI/London_Ward.shp")
shape_london_weekdays_w_polygon_available <- shape_london_w_polygon[shape_london_w_polygon$GSS_CODE %in% london_weekdays_grouped$GSS_CODE, ]
data_for_viz3 <- london_weekdays_grouped
data_for_viz3$geometry_polygon <- shape_london_weekdays_w_polygon_available$geometry 

data_for_viz3$estimated_random_effect <- model_weekdays_all_gamma$summary.random$id$mean
ggplot(data = data_for_viz3) +
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

p1 <- ggplot(data = data_for_viz3) +
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
data_for_viz3$estimated_random_effect_0.025 <- model_weekdays_all_gamma$summary.random$id$`0.025quant`
ggplot(data = data_for_viz3) +
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

p2 <- ggplot(data = data_for_viz3) +
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
data_for_viz3$estimated_random_effect_0.975 <- model_weekdays_all_gamma$summary.random$id$`0.975quant`
ggplot(data = data_for_viz3) +
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

p3 <- ggplot(data = data_for_viz3) +
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











# Visualization: log mean price and wo mean_dist and mean_attr_ind
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

