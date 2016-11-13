#############################
# Comparing Life Expectancy #
#############################

# Jonas Schöley, 2016-11-13

# Init --------------------------------------------------------------------

library(dplyr)
library(ggplot2)

# Data --------------------------------------------------------------------

# female life-tables 1970 by country from the
# Souce: "Human Mortality Database" (mortality.org)
load(file = "../data/hmd_female_lts1970.Rdata")

# life expectancy for each country
hmd_female_lts1970 %>%
  group_by(cntry) %>%
  summarize(e0 = ex[1]) -> ex

# A dotplot ---------------------------------------------------------------

ggplot(ex) +
  geom_point(aes(x = cntry, y = e0))

# Flip the axis to make labels readable -----------------------------------

ggplot(ex) +
  geom_point(aes(x = cntry, y = e0)) +
  coord_flip()

# An ordered dotplot ------------------------------------------------------

ggplot(ex) +
  geom_point(aes(x = reorder(cntry, e0), y = e0)) +
  coord_flip()

# Let's deceive -----------------------------------------------------------

# west versus east
ex %>%
  filter(cntry %in% c("DEUTW", "BGR", "RUS", "DEUTE", "POL", "CZE", "SVK")) %>%
  mutate(block = ifelse(cntry == "DEUTW", "west", "east")) %>%
  ggplot() +
  geom_point(aes(x = reorder(cntry, e0), y = e0, color = block),
             size = 5) +
  scale_y_continuous(limits = c(72.9, 73.7))