# File Header -------------------------------------------------------------

# File Name: tri_data_plot.R
# Created: 22 Jul 2021 by Tyler Weiglein
# Last Modified: 22 Jul 2021 by Tyler Weiglein

# Purpose: To create a plot for Toxic Release Inventory (TRI) data for RAAP.


# Preliminaries -----------------------------------------------------------

# Clear console and environment

cat("\014") 
rm(list = ls(all.names = TRUE))

# Load package(s)

library(lubridate)
library(tidyverse)

# Read in data

tri_data <- read_csv("data_raw/RAAP_TRI_data.csv")

