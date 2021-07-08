# File Header -------------------------------------------------------------

# File Name: incident_timeline_plot.R
# Created: 08 Jul 2021 by Tyler Weiglein
# Last Modified: 08 Jul 2021 by Tyler Weiglein

# Purpose: To create a timeline plot for incidents at RAAP.

# Code based off instructions given here: 
# https://benalexkeen.com/creating-a-timeline-graphic-using-r-and-ggplot2/

# **NOTE**: Currently using subset of incident data to test functionality.


# Preliminaries -----------------------------------------------------------

# Clear console and environment

cat("\014") 
rm(list = ls(all.names = TRUE))

# Load package(s)

library(lubridate)
library(tidyverse)

# Read in data

incident_data <- read_csv("incident_history/data_raw/incident_data_test.csv")


# Create Timeline Plot ----------------------------------------------------

# Modify data types in data frame

incident_data <- incident_data %>% 
  mutate(incident_type = as.factor(incident_type),
         date_occurred = mdy(date_occurred),
         date_public_notified = mdy(date_public_notified),
         month_year_occurred = paste(month(date_occurred), year(date_occurred)))  

# **NOTE**: Will also need to change amount to numeric

# Create positions and directions data frame

positions <- c(0.5, -0.5, 1.0, -1.0, 1.5, -1.5)
directions <- c(1, -1)

line_pos <- data.frame(
  "date_occurred" = unique(incident_data$date_occurred),
  "position" = rep(positions, length.out = length(unique(incident_data$date_occurred))),
  "direction" = rep(directions, length.out = length(unique(incident_data$date_occurred)))
)

# Join "incident_data" and "line_pos" data frames

incident_data <- left_join(incident_data, line_pos, by = "date_occurred")

# Offset text as necessary for incidents occurring in same month

text_offset <- 0.5

incident_data$month_count <- ave(incident_data$month_year_occurred == incident_data$month_year_occurred,
                                 incident_data$month_year_occurred,
                                 FUN = cumsum)
incident_data$text_position <- (incident_data$month_count * text_offset * incident_data$direction) + incident_data$position

# Create data frame for all months in plotting timeframe

month_buffer <- 2

month_date_range <- seq(min(incident_data$date_occurred) - months(month_buffer),
                        max(incident_data$date_occurred) + months(month_buffer),
                        by = "month")
month_format <- format(month_date_range, "%b")
month_df <- data.frame(month_date_range, month_format)

# Create data frame for all months in plotting timeframe

# year_date_range <- seq(min(incident_data$date_occurred) - months(month_buffer),
#                        max(incident_data$date_occurred) + months(month_buffer),
#                        by = "year")
# 
# ^Above code commented out b/c year 2021 excluded

year_date_range <- as.Date(
  intersect(
    ceiling_date(month_date_range, unit = "year"),
    floor_date(month_date_range, unit = "year")
  ), origin = "1970-01-01"
)
year_format <- format(year_date_range, "%Y")
year_df <- data.frame(year_date_range, year_format)

# Create timeline plot

timeline_plot <- ggplot(incident_data,
                        aes(x = date_occurred, y = 0, col = incident_type, label = chemical)) +
  labs(col = "Incident Type") +
  scale_color_discrete(labels = c("Fire", "Overflow", "Spill/Leak")) +
  theme_classic() +
  
  # Plot horizontal black line for timeline

  geom_hline(yintercept = 0,
             color = "black",
             size = 0.3) +
  
  # Plot vertical segment lines for milestones
  
  geom_segment(data = incident_data[incident_data$month_count == 1, ],
               aes(y = position, yend = 0, xend = date_occurred),
               color = "black",
               size = 0.2) +
  
  # Plot scatter points at zero and date
  
  geom_point(aes(y = 0),
             size = 3) +

  # Don't show axes, appropriately position legend
  
  theme(axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.position = "bottom"
        ) + 
  
  # Show text for each month
  
  geom_text(data = month_df,
            aes(x = month_date_range, y = -0.2, label = month_format),
            size = 4,
            vjust = 0.5,
            color = "black",
            angle = 90) +
  
  # Show year text
  
  geom_text(data = year_df,
            aes(x = year_date_range, y = -0.5, label = year_format, fontface = "bold"),
            size = 4,
            color = "black") +
  
  # Show text for each milestone
  
  geom_text(aes(y = text_position, label = chemical),
            col = "black",
            size = 4)



