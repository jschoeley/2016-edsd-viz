#' ---
#' title: "EDSD 2016: Lexis Surface Plots and the Effective Use of Color"
#' author: "Jonas Schöley"
#' date: "November 15, 2016"
#' output:
#'   github_document:
#'     toc: true
#' ---

library(ggplot2) # the way its done!
library(dplyr)   # data transformation

#'## Lexis Surfaces in GGplot

#' Lexis surfaces show the value of a third variable on a period-age-grid. If
#' the value of the third variable is given via colour the resulting plot is
#' known as "Heatmap" and used in many disciplines. In ggplot we produce
#' heatmaps using `geom_tile()` or `geom_rect()`. These geometries draw
#' rectangles at specified xy-positions. By default all rectangles are equal in
#' size. They can be coloured according to some variable in the data.
#'
#' `geom_rect()` is faster and produces smaller pdf's, while `geom_tile()`
#' allows to specify the dimensions of the rectangles making it useful for
#' data that does not come in single year period and age intervals.
#'
#' For now we will ignore the colouring aspect and just look at how ggplot draws
#' rectangles.

#'### 1x1 year data

#' We work with data from the [Human Mortality
#' Database](http://www.mortality.org) -- Swedish period mortality rates by sex.

swe <- read.csv("mortality_surface_sweden.csv")
swe

#' Only specifying x and y position and omitting colour puts a grey rectangle at
#' every xy position that appears in the data. The resulting plot gives us
#' information about the period-ages where we have mortality data on Swedish
#' females.

swe %>% filter(Sex == "Female") %>%
  ggplot() +
  geom_tile(aes(x = Year, y = Age))

#' When constructing Lexis surfaces it is a good idea to use isometric scales.
#' The distance corresponding to a single year should be the same on the x and
#' the y scales (a 1x1 rectangle should actually be a square). We can force such
#' an equality by adding a suitable coordinate layer.

swe %>% filter(Sex == "Female") %>%
  ggplot() +
  geom_tile(aes(x = Year, y = Age)) +
  coord_equal()

#' By default the small rectangles have a width and height of 1 scale unit and
#' are drawn over the mid-points of the corresponding x and y values.

swe %>% filter(Sex == "Female") %>%
  ggplot() +
  geom_tile(aes(x = Year, y = Age), colour = "white") +
  scale_x_continuous(breaks = 1800:1810) +
  scale_y_continuous(breaks = 100:110) +
  coord_equal(xlim = c(1800, 1810), ylim = c(100, 110))

#' Shifting the data by 0.5 in x and y aligns things neatly.

swe %>% filter(Sex == "Female") %>%
  mutate(Year = Year + 0.5, Age = Age + 0.5) %>%
  ggplot() +
  geom_tile(aes(x = Year, y = Age), colour = "white") +
  scale_x_continuous(breaks = 1800:1810) +
  scale_y_continuous(breaks = 100:110) +
  coord_equal(xlim = c(1800, 1810), ylim = c(100, 110))

#'### nxm year data

#' If our data does not come in single year and age groups we have to adjust
#' the `width` and/or `height` of the rectangles. `width` and `height` are
#' regular aesthetics and can be mapped to variables in the data.

cod <- read.csv("cod.csv")
cod

#' The Cause of Death data features age groups of different sizes (1, 4, or 5
#' years). This is how it looks like if we plot it without any regard to the
#' size of the age groups.

cod %>% filter(Sex == "Female") %>%
  mutate(Year = Year + 0.5) %>%
  ggplot() +
  geom_tile(aes(x = Year, y = Age),
            colour = "white") +
  coord_equal()

#' Now we shift the rectangles away from the age midpoint and scale them
#' in height according to the width of the age group.

cod %>% filter(Sex == "Female") %>%
  mutate(Year = Year + 0.5, Age = Age + w/2) %>%
  ggplot() +
  geom_tile(aes(x = Year, y = Age, height = w),
            colour = "white") +
  coord_equal()

#'### Discrete Period and Age Scales

#' If we use discrete axis (happens automatically if we supply a non-numeric
#' variable to the x or y aesthetic) we loose any control over the placement of
#' the age or period groups. They will be equally spaced along the axis.

cod %>% filter(Sex == "Female") %>%
  mutate(Year = Year + 0.5, Age = AgeGr) %>%
  ggplot() +
  geom_tile(aes(x = Year, y = Age), colour = "white") +
  coord_equal()

#' **Avoid character or factor variables as your period or age groups.**
#' Whenever possible go with numeric "Start of Interval" and "Interval Width"
#' variables.

#'## Sequential Colour Scales: Plotting Magnitudes

#' If we plot magnitudes we would like to use a colour scale which has an
#' intrinsic ordering to it. Scales that vary from dark to light are suitable
#' and we call them "sequential". `scale_fill_brewer(type = "seq")` provides
#' you with such a scale.

breaks_mx <- c(0, 0.0001, 0.001, 0.01, 0.1, Inf)
swe %>%
  mutate(Year = Year + 0.5, Age = Age + 0.5,
         mx_cut = cut(mx, breaks = breaks_mx)) %>%
  ggplot() +
  geom_tile(aes(x = Year, y = Age, fill = mx_cut)) +
  scale_fill_brewer(type = "seq") +
  facet_wrap(~Sex, ncol = 1) +
  guides(fill = guide_legend(reverse = TRUE)) +
  coord_equal()

swe %>%
  mutate(Year = Year + 0.5, Age = Age + 0.5) %>%
  ggplot() +
  geom_tile(aes(x = Year, y = Age, fill = mx)) +
  scale_fill_distiller(type = "seq", palette = "PuBuGn", direction = 1,
                       breaks = breaks_mx,
                       trans = "log10",
                       values = c(0, 0.3, 0.4, 0.5, 0.6, 1),
                       limits = c(0.001, 0.5),
                       oob = scales::squish) +
  facet_wrap(~Sex, ncol = 1) +
  coord_equal()

#'## Divergent Colour Scales: Plotting Differences & Proportions

breaks_prop_mx <- c(0, 0.5, 0.7, 0.9, 1.1, 1.3, 1.5, Inf)
swe %>%
  mutate(Year = Year + 0.5, Age = Age + 0.5) %>%
  select(-Dx, -Nx) %>%
  tidyr::spread(key = Sex, value = mx) %>%
  mutate(fm_prop_mx = Female / Male,
         fm_prop_mx_disc = cut(fm_prop_mx, breaks_prop_mx)) %>%
  ggplot() +
  geom_tile(aes(x = Year, y = Age, fill = fm_prop_mx_disc)) +
  scale_fill_brewer(type = "div", palette = 5, direction = -1) +
  guides(fill = guide_legend(reverse = TRUE)) +
  coord_equal() +
  theme_dark()

swe %>%
  mutate(Year = Year + 0.5, Age = Age + 0.5) %>%
  select(-Dx, -Nx) %>%
  tidyr::spread(key = Sex, value = mx) %>%
  mutate(fm_diff_mx = Female / Male) %>%
  ggplot() +
  geom_tile(aes(x = Year, y = Age, fill = fm_diff_mx)) +
  # takes 6 colours from a brewer palette and interpolates
  scale_fill_distiller(type = "div",
                       palette = "RdBu",
                       trans = "log2",
                       limits = c(0.5, 2),
                       oob = scales::squish) +
  coord_equal()

#'## Qualitative Colour Scales: Plotting Group Membership

cod %>%
  mutate(Year = Year + 0.5, Age = Age + w/2) %>%
  ggplot() +
  geom_tile(aes(x = Year, y = Age, height = w, fill = COD)) +
  coord_equal() +
  facet_wrap(~Sex, ncol = 2)

#'## Further Reading

#' - [Brilliant color advice from NASA](earthobservatory.nasa.gov/blogs/elegantfigures/2013/08/05/subtleties-of-color-part-1-of-6)
#' - [Color scale generator](http://vrl.cs.brown.edu/color)
#' - [Color scales for data-viz](colorbrewer2.org)
#' - Brewer, Cynthia A. 1994. “Guidelines for Use of the Perceptual Dimensions
#'     of Color for Mapping and Visualization.” In SPIE, edited by Jan Bares,
#'     2171:54–63. doi:10.1117/12.175328.

sessionInfo()

#' cc-by Jonas Schöley 2016
