library(dplyr)
library(readr)

# verified supercentenarian deaths by country and year
# Source: "International Database on Longevity" (supercentenarians.org)
sapply(list.files(path = "./idl/"),
       function (x) read_delim(paste0("./idl/", x),
                               skip = 1,
                               col_names = c("id", "age_y", "age_d", "dob", "dod", "validation"),
                               col_types = "ii-i---ccc", delim = ";"),
       simplify = FALSE, USE.NAMES = TRUE) -> idl

# merge all data
bind_rows(idl, .id = "file") %>%
  # specify integer year dates
  mutate(dob = as.integer(gsub("^[[:digit:]]{2}/[[:digit:]]{2}/", "", dob)),
         dod = as.integer(gsub("^[[:digit:]]{2}/[[:digit:]]{2}/", "", dod))) %>%
  # add age at rational
  mutate(age_y_frac = age_d/365.2422) %>%
  # only include perfectly validated cases
  filter(validation == "A") %>%
  # find the maximum recorded age at death for each year
  group_by(dod) %>%
  filter(age_y_frac == max(age_y_frac)) %>%
  ungroup() %>%
  # group the data like dong etal.
  mutate(group = ifelse(dod < 1995, "a", "b")) -> idl_mrad

save(idl_mrad, file = "./idl_mrad.Rdata")
