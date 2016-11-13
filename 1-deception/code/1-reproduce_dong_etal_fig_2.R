#############################################
# Reproducing Figure 2 of Dong etal. (2016) #
#############################################

# Jonas Schöley, 2016-11-13

# Init --------------------------------------------------------------------

library(dplyr)
library(ggplot2)

# Data --------------------------------------------------------------------

# maximum reported annual ages at death as derived from
# Source: "International Database on Longevity" (supercentenarians.org)
load(file = "../data/idl_mrad.Rdata")

# 1) first plot -----------------------------------------------------------

# the data we use, must be a data frame
ggplot(data = idl_mrad) +
# the geometry (visual marks / "general plot type")
# and the aestetics (mappings from data dimensions to visual dimensions)
geom_point(aes(x = dod, y = age_y_frac))

# 2) add 1:1 scale ratio --------------------------------------------------

ggplot(data = idl_mrad) +
  geom_point(aes(x = dod, y = age_y_frac)) +
  # specify a "coordinate system" where 1 unit change on the y-scale
  # is represented by the same physical length as 1 unit change on the
  # x-scale. Advantage: Does not over or under pronounce spread along the
  # the x or y scales. Perfectly proportionate (x,y) data will be on a 45
  # degree diagonal.
  coord_fixed(ratio = 1)

# 3) skew ratio between scales to pronounce change across y-scale ---------

ggplot(data = idl_mrad) +
  geom_point(aes(x = dod, y = age_y_frac)) +
  # one unit change on the y scale corresponds to 2.5 unit changes on
  # the x scale
  coord_fixed(ratio = 2.5)

# 4) colour the data points to guide the eye ------------------------------

ggplot(data = idl_mrad) +
  # map the variable "group" to the aesthetic "color"
  geom_point(aes(x = dod, y = age_y_frac, color = group)) +
  coord_fixed(ratio = 2.5) +
  # manually specify the colors to use for each level in the
  # variable "group"
  scale_color_manual(values = c(a = "#5B9BD5", b = "#ED7D31"))

# 5) bigger points, so the colors stand out -------------------------------

ggplot(data = idl_mrad) +
  # if we "set" an aesthetic to a constant value (instead
  # of mapping it to data) it needs to go outside of the
  # `aes()` command
  geom_point(aes(x = dod, y = age_y_frac, color = group), size = 5) +
  coord_fixed(ratio = 2.5) +
  scale_color_manual(values = c(a = "#5B9BD5", b = "#ED7D31"))

# 6) add regression lines to guide the eye --------------------------------

ggplot(data = idl_mrad) +
  geom_point(aes(x = dod, y = age_y_frac, color = group), size = 5) +
  # by default a "loess" smoother is fitted to the specified x and y values
  geom_smooth(aes(x = dod, y = age_y_frac)) +
  coord_fixed(ratio = 2.5) +
  scale_color_manual(values = c(a = "#5B9BD5", b = "#ED7D31"))

# 7) instead of loess smoother, add segmented linear regression -----------

ggplot(data = idl_mrad) +
  geom_point(aes(x = dod, y = age_y_frac, color = group), size = 5) +
  # separate linear (`method = "lm"`) regressions by group as
  # specified in color no standard errors plotted
  geom_smooth(aes(x = dod, y = age_y_frac, color = group),
              method = "lm", se = FALSE) +
  coord_fixed(ratio = 2.5) +
  scale_color_manual(values = c(a = "#5B9BD5", b = "#ED7D31"))

# 8) round the MRADs to lower integer age ---------------------------------

ggplot(data = idl_mrad) +
  geom_point(aes(x = dod, y = floor(age_y_frac), color = group), size = 5) +
  # separate linear (`method = "lm"`) regressions by group as
  # specified in color no standard errors plotted
  geom_smooth(aes(x = dod, y = floor(age_y_frac), color = group),
              method = "lm", se = FALSE) +
  coord_fixed(ratio = 2.5) +
  scale_color_manual(values = c(a = "#5B9BD5", b = "#ED7D31"))

# 9) the final touch ------------------------------------------------------

dong <-
  ggplot(data = idl_mrad,
         # aesthetics specified in the `ggplot()` command
         # are shared with all geoms
         aes(x = dod, y = floor(age_y_frac), color = group)) +
  geom_point(size = 5) +
  geom_smooth(method = "lm", se = FALSE) +
  coord_fixed(ratio = 2.5) +
  scale_color_manual(values = c(a = "#5B9BD5", b = "#ED7D31"),
                     guide = FALSE) +
  scale_y_continuous(name = "MRAD",
                     # draw y-scale from 108 to 124
                     limits = c(108, 124),
                     # set tickmarks at 108 to 124 in two year intervals
                     breaks = seq(108, 124, 2),
                     # draw exactly to the limits and not further
                     expand = c(0, 0)) +
  scale_x_continuous(name = "Year",
                     limits = c(1960, 2010),
                     expand = c(0,0)) +
  # change the general plot theme theme
  theme_classic() +
  theme(
    # make text larger
    text = element_text(size = 16),
    # add x and y axes
    axis.line.x = element_line(),
    axis.line.y = element_line()
  )

# an alternative plot of the same data ------------------------------------

jon <-
  ggplot(data = idl_mrad, aes(x = dod, y = age_y_frac)) +
  geom_point(size = 5) +
  coord_fixed(ratio = 1) +
  scale_y_continuous(name = "MRAD",
                     # draw y-scale from 108 to 124
                     limits = c(108, 124),
                     # set tickmarks at 108 to 124 in two year intervals
                     breaks = seq(108, 124, 2),
                     # draw exactly to the limits and not further
                     expand = c(0, 0)) +
  scale_x_continuous(name = "Year",
                     limits = c(1960, 2010),
                     expand = c(0,0)) +
  # change the general plot theme theme
  theme_classic() +
  theme(
    # make text larger
    text = element_text(size = 16),
    # add x and y axes
    axis.line.x = element_line(),
    axis.line.y = element_line()
  )

# compare -----------------------------------------------------------------

dong
jon