######################################
# The decline of infectious diseases #
######################################

# Jonas Schöley, 2016-11-13

# Init --------------------------------------------------------------------

library(dplyr)
library(ggplot2)

# Data --------------------------------------------------------------------

# Mortality rates by cause of death over time by sex in France
# Souce: "INED" (https://www.ined.fr/en/everything_about_population/data/online-databases/death_causes_since_1925/)
cod <- read.csv("../data/ined-cod-fra-1925-1999-rates.csv", skip = 19)

cod %>%
  filter(sex == "total", cod == "001*-139*", age != "total") %>%
  group_by(year) %>%
  mutate(age_start = c(0, 1, seq(5, 100, 5)),
         age_width = c(diff(age_start), 5)) -> infect

# Lexis surface -----------------------------------------------------------

ggplot(infect) +
  geom_tile(aes(x = year, width = 1,
                y = age_start, height = age_width,
                fill = mx/1000)) +
  scale_fill_distiller(type = "seq", palette = "PuBuGn", direction = 1,
                       limits = c(0.0001, 0.5))

ggplot(infect) +
  geom_tile(aes(x = year, width = 1,
                y = age_start, height = age_width,
                fill = mx/1000)) +
  scale_fill_distiller(type = "seq", palette = "PuBuGn", direction = 1,
                       limits = c(0.0001, 0.5),
                       oob = scales::squish)

ggplot(infect) +
  geom_tile(aes(x = year, width = 1,
                y = age_start, height = age_width,
                fill = mx/1000)) +
  scale_fill_distiller(type = "seq", palette = "PuBuGn", direction = 1,
                       limits = c(0.0001, 0.2),
                       oob = scales::squish)

ggplot(infect) +
  geom_tile(aes(x = year, width = 1,
                y = age_start, height = age_width,
                fill = mx/1000)) +
  scale_fill_distiller(type = "seq", palette = "PuBuGn", direction = 1,
                       trans = "log10",
                       limits = c(0.0001, 0.2),
                       oob = scales::squish)