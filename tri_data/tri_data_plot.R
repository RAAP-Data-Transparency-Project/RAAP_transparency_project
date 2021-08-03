# File Header -------------------------------------------------------------

# File Name: tri_data_plot.R
# Created: 22 Jul 2021 by Tyler Weiglein
# Last Modified: 03 Aug 2021 by Tyler Weiglein

# Purpose: To create a plot for Toxic Release Inventory (TRI) data for RAAP.


# Preliminaries -----------------------------------------------------------

# Clear console and environment

cat("\014") 
rm(list = ls(all.names = TRUE))

# Load package(s)

library(lubridate)
library(scales)
library(tidyverse)

# Read in data

tri_data <- read_csv("tri_data/data_raw/RAAP_TRI_data.csv")


# Create TRI Data Plot ----------------------------------------------------

tri_data <- tri_data %>% 
  select(c(REPORTING_YEAR, AIR_TOTAL_RELEASE, WATER_TOTAL_RELEASE, LAND_TOTAL_RELEASE, TOTAL_OFF_SITE_RELEASE)) %>%
  rename(year = REPORTING_YEAR,
         air = AIR_TOTAL_RELEASE,
         water = WATER_TOTAL_RELEASE,
         land = LAND_TOTAL_RELEASE,
         off_site = TOTAL_OFF_SITE_RELEASE) %>% 
  pivot_longer(cols = c(air, water, land, off_site),
               names_to = "media",
               values_to = "release_lbs") %>% 
  mutate(media = ordered(as.factor(media), levels = c("air", "water", "land", "off_site"))) %>% 
  group_by(year, media) %>% 
  summarize(tot_release_lbs = sum(release_lbs)) %>% 
  filter(year >= 2010)

tri_data_plot <- ggplot(tri_data, aes(x = year, y = tot_release_lbs, fill = media)) +
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity") +
  scale_fill_manual(labels = c("Air", "Water", "Land", "Off-Site"),
                    values = c("#CCE3FC", "#1A6692", "#395420", "#DA6D1D")) +
  scale_y_continuous(labels = comma) +
  labs(title = "RAAP TRI Releases by Year (2010-2019)",
       x = "Year",
       y = "Releases (lb)",
       fill = "Key:") +
  theme_bw()
  
ggsave("tri_data/fig/tri_data_plot.png", tri_data_plot, width = 8, height = 5, units = "in", dpi = 600)