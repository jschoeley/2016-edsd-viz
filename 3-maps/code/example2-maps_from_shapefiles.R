library(maptools)
library(ggplot2)
library(dplyr)
library(gpclib)
library(rgeos)

# read data on differences in median age 1991-2013 by country
load("../data/eu_medianage_diff_1991_2013.Rdata")

# read shapefile of the global country borders
# source: http://ec.europa.eu/eurostat/cache/GISCO/geodatafiles/CNTR_2014_03M_SH.zip
world_map_nuts0 <- readShapeSpatial("../data/CNTR_RG_03M_2014.shp")

# save the country codes and labels
cntr_code <- row.names(world_map_nuts0@data)
cntr_lab  <- as.character(world_map_nuts0@data$CNTR_ID)

# convert map polygon object to data frame
world_map_nuts0 %>%
  # do some transformations in order to deal with "bad polygons"
  gSimplify(tol = 1e-5) %>%
  gBuffer(byid = TRUE, width = 0) %>%
  # now we can transform to dataframe
  fortify(region = "CNTR_ID") %>%
  # relabel the countries
  mutate(id = factor(id,
                     levels = cntr_code,
                     labels = cntr_lab)) -> world_map_nuts0_df

# merge demographic data with map data by index
left_join(x = world_map_nuts0_df,
          y = eu_medianage_diff_1991_2013,
          by = c("id" = "GEO")) -> age_map

# plot map of 2013-1991 differences in median age in EU28
ggplot(age_map,
       aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = MEDAGEPOP_DIFF_1991_2013_DISC), colour = "black", size = 0.1) +
  scale_x_continuous(name = "", breaks = NULL) +
  scale_y_continuous(name = "", breaks = NULL) +
  scale_fill_brewer(name = "", type = "seq", palette = "RdPu", na.value = "grey") +
  coord_map(projection = "conic", lat0 = 55, xlim = c(-10, 40), ylim = c(33, 72)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#B3D6FF", colour = NA))
