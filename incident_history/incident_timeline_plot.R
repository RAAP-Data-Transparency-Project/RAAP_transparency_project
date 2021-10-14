# File Header -------------------------------------------------------------

# File Name: incident_timeline_plot.R
# Created: 08 Jul 2021 by Tyler Weiglein
# Last Modified: 14 Oct 2021 by Tyler Weiglein

# Purpose: To create a timeline plot for incidents at RAAP.

# Code based off instructions given here: 
# https://benalexkeen.com/creating-a-timeline-graphic-using-r-and-ggplot2/


# Preliminaries -----------------------------------------------------------

# Clear console and environment

cat("\014") 
rm(list = ls(all.names = TRUE))

# Load package(s)

library(ggtext)
library(lubridate)
library(tidyverse)

# Read in data

incident_data <- read_csv("incident_history/data_raw/raap_incident_data_2018_sep2021.csv")


# Create Timeline Plot ----------------------------------------------------

# Modify data types in data frame

incident_data <- incident_data %>%
  select(release_type, date_occurred, description) %>% 
  mutate(release_type = as.factor(release_type),
         date_occurred = mdy(date_occurred),
         month_year_occurred = paste(month(date_occurred), year(date_occurred))) %>% 
  filter(release_type != "Sediment")

# Add line breaks to description names to make sure descriptions do not overlap on timeline

incident_data$description <- str_replace(incident_data$description,
                                         "Anhydrous ammonia",
                                         "Anhydrous\nammonia")

incident_data$description <- str_replace(incident_data$description,
                                         "Propellant slurry",
                                         "Propellant\nslurry")

incident_data$description <- str_replace(incident_data$description,
                                         "Oil/Hydraulic oil",
                                         "Oil/\nHydraulic oil")

incident_data$description <- str_replace(incident_data$description,
                                         "Crowder tank flooding",
                                         "Crowder tank\nflooding")

incident_data$description <- str_replace(incident_data$description,
                                         "Caustic rinse water \\(pH ~ 11\\)",
                                         "Caustic\nrinse water")

incident_data$description <- str_replace(incident_data$description,
                                         "Reclamation water",
                                         "Reclamation\nwater")

incident_data$description <- str_replace(incident_data$description,
                                         "Industrial wastewater \\(\"minute\" quantities of nitrocellulose\\)",
                                         "Industrial\nwastewater")

incident_data$description <- str_replace(incident_data$description,
                                         "Industrial wastewater \\(77 ppm nitroglycerin\\)",
                                         "Industrial\nwastewater")

incident_data$description <- str_replace(incident_data$description,
                                         "Brine water",
                                         "Brine\nwater")

incident_data$description <- str_replace(incident_data$description,
                                         "Industrial wastewater$",
                                         "Industrial\nwastewater")

incident_data$description <- str_replace(incident_data$description,
                                         "Nitric acid$",
                                         "Nitric\nacid")

incident_data$description <- str_replace(incident_data$description,
                                         "Industrial residuals",
                                         "Industrial\nresiduals")

incident_data$description <- str_replace(incident_data$description,
                                         "Nitric acid/sulfuric acid mixture",
                                         "Nitric acid/\nsulfuric acid\nmixture")

incident_data$description <- str_replace(incident_data$description,
                                         "Industrial process wastewater and sewage",
                                         "Industrial\nprocess wastewater\nand sewage")

incident_data$description <- str_replace(incident_data$description,
                                         "Diesel fuel",
                                         "Diesel\nfuel")

incident_data$description <- str_replace(incident_data$description,
                                         "Diethyl ether",
                                         "Diethyl\nether")

incident_data$description <- str_replace(incident_data$description,
                                         "Acidic wastewater",
                                         "Acidic\nwastewater")

incident_data$description <- str_replace(incident_data$description,
                                         "Industrial process wastewater",
                                         "Industrial process\nwastewater")

# Create positions and directions data frame

positions <- c(0.85, -0.85, 1.7, -1.7, 2.55, -2.55)
directions <- c(1, -1)

line_pos <- data.frame(
  "position" = rep(positions, length.out = length(incident_data$date_occurred)),
  "direction" = rep(directions, length.out = length(incident_data$date_occurred))
)

# Join "incident_data" and "line_pos" data frames

incident_data <- bind_cols(incident_data, line_pos)

# Create data frame for all months in plotting timeframe

month_buffer <- 2

month_date_range <- seq(ym(format(min(incident_data$date_occurred), "%Y-%m")) - months(month_buffer),
                        ym(format(max(incident_data$date_occurred), "%Y-%m")) + months(month_buffer - 1),
                        by = "month")
month_format <- format(month_date_range, "%b")
month_df <- data.frame(month_date_range, month_format)

# Create data frame for all years in plotting timeframe

year_date_range <- as.Date(
  unique(floor_date(month_date_range, unit = "year")),
  origin = "1970-01-01"
)
year_format <- format(year_date_range, "%Y")
year_df <- data.frame(year_date_range, year_format)

# Create timeline plot

timeline_plot <- ggplot(incident_data,
                        aes(x = date_occurred, y = 0, label = description)) +
  labs(fill = "Incident Type:") +
  scale_fill_manual(values = c("#619CFF", "#F8766D", "#00BA38"),
                    labels = c("Chemical Spill", "Fire", "Other")) +
  theme_classic() +
  
  # Plot horizontal black line for timeline

  geom_hline(yintercept = 0,
             color = "black",
             size = 0.3) +
  
  # Specify y-axis limits
  
  ylim(1.3 * min(incident_data$position), 1.3 * max(incident_data$position)) +
  
  # Plot vertical segment lines for incidents
  
  geom_segment(aes(y = position, yend = 0, xend = date_occurred),
               color = "black",
               size = 0.2) +
  
  # Plot scatter points at zero and date
  
  geom_point(aes(y = 0, fill = release_type),
             shape = 21,
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
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 12),
        legend.position = "bottom") + 
  
  # Show text for each month
  
  geom_richtext(data = month_df,
                aes(x = month_date_range, y = -0.1, label = month_format),
                size = 3.25,
                hjust = 1,
                vjust = 0.5,
                color = "black",
                angle = 90,
                label.padding = unit(-0.03, "lines"),
                label.size = NA) +
  
  # Show year text
  
  geom_label(data = year_df,
            aes(x = year_date_range, y = -0.65, label = year_format, fontface = "bold"),
            size = 4,
            color = "black",
            label.padding = unit(0.15, "lines"),
            label.size = NA) +
  
  # Show text for each incident

  geom_label(data = filter(incident_data, position > 0),
             aes(y = position, label = description),
             col = "black",
             fill = "gray97",
             label.padding = unit(0.3, "lines"),
             size = 3.25,
             vjust = 0) +

# Show text for each incident (below timeline)

  geom_label(data = filter(incident_data, position < 0),
             aes(y = position, label = description),
             col = "black",
             fill = "gray97",
             label.padding = unit(0.3, "lines"),
             size = 3.25,
             vjust = 1)

# Save figure

ggsave("incident_history/fig/incident_timeline_plot.png", timeline_plot, width = 12, height = 6, units = "in", dpi = 1200)