# File Header -------------------------------------------------------------

# File Name: tri_data_table.R
# Created: 29 Jul 2021 by Tyler Weiglein
# Last Modified: 29 Jul 2021 by Tyler Weiglein

# Purpose: To create a table for Toxic Release Inventory (TRI) data for RAAP.


# Preliminaries -----------------------------------------------------------

# Clear console and environment

cat("\014") 
rm(list = ls(all.names = TRUE))

# Load package(s)

library(openxlsx)
library(tidyverse)

# Read in data

tri_data <- read_csv("tri_data/data_raw/RAAP_TRI_data.csv")


# Create TRI Data Table ---------------------------------------------------

tri_data_table <- tri_data %>% 
  select(c(CHEM_NAME, REPORTING_YEAR, AIR_TOTAL_RELEASE, WATER_TOTAL_RELEASE, LAND_TOTAL_RELEASE, TOTAL_OFF_SITE_RELEASE)) %>%
  rename(chem_name = CHEM_NAME,
         year = REPORTING_YEAR,
         Air = AIR_TOTAL_RELEASE,
         Water = WATER_TOTAL_RELEASE,
         Land = LAND_TOTAL_RELEASE,
         "Off-site" = TOTAL_OFF_SITE_RELEASE)

tri_data_table$chem_name <- str_replace_all(tri_data_table$chem_name,
                "Nitrate compounds \\(water dissociable; reportable only when in aqueous solution\\)",
                "Nitrate compounds")

tri_data_table$chem_name <- str_replace_all(tri_data_table$chem_name,
                "Hydrochloric acid \\(acid aerosols including mists  vapors  gas  fog  and other airborne forms of any particle size\\)",
                "Hydrochloric acid")

tri_data_table$chem_name <- str_replace_all(tri_data_table$chem_name,
                "Sulfuric acid \\(acid aerosols including mists  vapors  gas  fog  and other airborne forms of any particle size\\)",
                "Sulfuric acid")

tri_data_table$chem_name <- str_replace_all(tri_data_table$chem_name,
                "LEAD  and LEAD COMPOUNDS",
                "Lead compounds")

tri_data_table$chem_name <- str_replace_all(tri_data_table$chem_name,
                "COPPER  and COPPER COMPOUNDS",
                "Copper compounds")

tri_data_table <- tri_data_table %>% 
  pivot_longer(cols = c(Air, Water, Land, "Off-site"),
               names_to = "media",
               values_to = "release_lbs") %>% 
  mutate(media = as.factor(media)) %>% 
  filter(year >= 2010,
         release_lbs > 0) %>% 
  group_by(chem_name, media) %>% 
  summarize(tot_release_lbs = sum(release_lbs)) %>% 
  ungroup() %>% 
  mutate(pct_release = tot_release_lbs / sum(tot_release_lbs) * 100) %>% 
  arrange(desc(pct_release))

colnames(tri_data_table) <- c("Chemical Name", "Release Medium", "Total Releases [2010-2019] (lb)", "Relative Contribution [2010-2019] (%)")

write.xlsx(tri_data_table, "tri_data/table/tri_data_table.xlsx")