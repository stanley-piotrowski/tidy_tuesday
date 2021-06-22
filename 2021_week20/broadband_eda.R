# Intro ----
# 2021 Week 20 exploring broadband internet disparities in the US

# Setup ----
library(tidyverse)
library(tidytuesdayR)
library(janitor)
library(ggtext)
library(ggridges)
library(tigris) # Working with shape files
library(zipcodeR)
library(paletteer) # for elegant plotting on maps
library(viridis)
library(randomcoloR) # create many different colors to distinguish counties
library(scales)
library(sf)
library(patchwork)

# Set ggplot2 theme ----
my_theme <- theme(
  
  # Customize panels
  panel.background = element_rect(fill = "white", color = "black"),
  panel.grid = element_blank(),
  
  # Customize titles, axes, and caption
  plot.title = element_markdown(size = 20),
  plot.subtitle = element_markdown(),
  plot.caption = element_text(face = "italic"),
  axis.title.x = element_text(size = 14), 
  axis.title.y = element_text(size = 14)
  
  # Adjust y-axis scale
)

# Data ----
# Load the larger zip code data from the tidytuesdayR package
tuesdata <- tidytuesdayR::tt_load("2021-05-11")
broadband <- tuesdata$broadband

# For the detailed analysis, we'll use the broadband zipcode data
broadband_zip <- tuesdata$broadband_zip %>% 
  clean_names()

# Explore data
glimpse(broadband)

# Data contains five columns for the state (two letter code), county ID (double), county name (full name), broadband per fcc (percent of people per county with access to broadband at the end of 2017), and broadband usage (percent of people per county that actually use the broadband internet)

# Clean up column names ----
broadband <- broadband %>% 
  clean_names()

# It looks like there are some states with missing data
# Summary plot to visualize the distribution of missing data across states
missing_data_by_state <- broadband %>% 
  mutate(broadband_availability_per_fcc = str_replace(broadband_availability_per_fcc, 
                                                      "-", "NA")) %>% 
  filter(broadband_availability_per_fcc == "NA") %>% 
  group_by(st) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(reorder(st, -n), n)) +
  geom_hline(yintercept = c(5, 10),
             linetype = "dotted", 
             color = "black") +
  geom_bar(stat = "identity",
           fill = "lightskyblue", 
           alpha = 0.65) +
  expand_limits(x = 0, y = 15) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "State", 
       y = "Count", 
       title = "<strong>Missing Broadband Data by State</strong>", 
       subtitle = "Alaska (AK) had the highest number of counties (15) with missing broadband internet data
                in 2017,<br>followed by Texas (TX; 6), Virginia (VA; 3), and Idaho (ID; 2).  The remaining
                states had missing<br>data in one county.", 
       caption = "Source: Microsoft (https://github.com/microsoft/USBroadbandUsagePercentages)") +
  my_theme

# Take a look at the plot before saving
missing_data_by_state

# Density plots ----
# Remove missing data
broadband_nonmissing <- broadband %>% 
  mutate(broadband_availability_per_fcc = str_replace(broadband_availability_per_fcc, 
                                                      "-", "NA")) %>% 
  filter(broadband_availability_per_fcc != "NA")

# Explore distribution of broadband access across states
broadband_nonmissing %>% 
  ggplot(aes(broadband_availability_per_fcc, st, group = st)) +
  geom_density_ridges(scale = 2) +
  scale_x_discrete(breaks = c("0.00", "0.25", "0.50", "0.75", "1.00"), 
                   expand = c(0, 0))

# Explore NJ data ----
# Here, we'll use the tigris package to grab shape files from the TIGER/Line repository from the US Census Bureau
# Briefly, TIGER stands for Topographically Integrated Geographic Encoding and Referencing system used by the Census Bureau to describe land features like county lines, roads, rivers, etc

# We'll use the counties() function to download a shape file from the US Census Bureau
counties_nj <- counties(state = "NJ", cb = FALSE)

# The default behavior for cb = TRUE is to fetch the cartographic boundary file from the TIGER/Line database, which is the most detailed and also includes the legal boundary extending 3 miles from the coastline
# Get an overview of what the plot will look like with all counties where data are available
ggplot(counties_nj) +
  geom_sf()

# Now, perform another geom_join() with the county_geometry data with the broadband data, since the broadband data are per county
broadband_by_county <- geo_join(spatial_data = counties_nj, 
         data_frame = broadband, 
         by_sp = "NAMELSAD", 
         by_df = "county_name") %>% 
  mutate(broadband_availability_per_fcc = as.numeric(broadband_availability_per_fcc), 
         broadband_usage = as.numeric(broadband_usage))

# Plot NJ broadband availability ----
# Now that we've created a data frame with all of the broadband data and the multipolygon geometry data for each county, we can start plotting to look at the relationship between broadband availability and usage

# Plot 
broadband_availability_plot <- ggplot(broadband_by_county, aes(fill = broadband_availability_per_fcc)) +
  geom_sf(color = "black", 
          size = 0.2) +
  scale_fill_viridis_c(name = "Availability") +
  theme_void()

# Plot NJ broadband usage ----
# Plot
broadband_usage_plot <- ggplot(broadband_by_county, 
       aes(fill = broadband_usage)) +
  geom_sf(color = "black", 
          size = 0.2) +
  scale_fill_viridis_c(name = "Usage", 
                       breaks = seq(0, 1, 0.25), 
                       n.breaks = 5, 
                       limits = c(0, 1)) +
  theme_void()

# Put plots together ----
# Use patchwork to put plots together
broadband_combined_plot <- broadband_availability_plot +
  broadband_usage_plot + 
  plot_annotation(title = "Broadband availability and usage in New Jersey", 
                  subtitle = "Availability (A) and usage (B) are defined as the percentage of people per county with\nbroadband internet; FCC guidelines define broadband at speeds of 25 Mbps (download)\nand 3 Mbps (upload)",
                  tag_levels = "A", 
                  tag_suffix = ")", 
                  caption = "Broadband data source: Microsoft (https://github.com/microsoft/USBroadbandUsagePercentages)", 
                  theme = theme(plot.title = element_text(face = "bold", size = 18)))

# Detailed zipcode analysis ----
# The code below is for detailed zipcode analysis, which may be revisited at another time
# The Tidy Tuesday data on broadband usage are per county only

# Get the zctas (Zip Code Tabulated Areas) for NJ
# Note, this won't take as long as the option "cb = FALSE"
# ZCTA is a geographic dataset that approximates zip codes, because zip codes themselves may change and aren't always tied to the same geographic locations
# Note, the default year is 2019, but those data are not available for some states
# For example, ZCTA data are only available for 2000 and 2010 in NJ
zctas_nj <- zctas(state = "NJ", 
                  year = 2010)

# Extract zip codes for NJ
zipcodes_nj <- search_state("NJ") %>% 
  mutate(zcta_test = is_zcta(zipcode), 
         zipcode = as.numeric(zipcode))

# Merge the broadband and zipcode code NJ data
# Note, pad the postal code with a 0 on the left side
broadband_data_nj <- left_join(broadband_zip, zipcodes_nj, 
                               by = c("postal_code" = "zipcode")) %>% 
  filter(st == "NJ") %>% 
  mutate(postal_code = as.character(postal_code), 
         postal_code = str_pad(postal_code, 5, side = "left", pad = "0"))

# Now that we have the broadband data for each county, we need to merge with the ZCTA object to get the multipolygon geometry data in order to draw the maps
# To accomplish this task, use the geo_join() function to merge the spatial features data frame zctas_nj with the regular data frame broadband_data_nj
broadband_spatial_nj <- geo_join(zctas_nj, broadband_data_nj, 
                                 "ZCTA5CE10", "postal_code", 
                                 how = "inner")

# Finally, we need to merge this with the original broadband dataset in order to evaluate the relationship between population density and broadband data availability 
broadband_spatial_nj <- geo_join(spatial_data = broadband_spatial_nj, 
                                 data_frame = broadband, 
                                 by_sp = "county", 
                                 by_df = "county_name") %>% 
  clean_names() %>% 
  mutate(broadband_availability_per_fcc = as.numeric(broadband_availability_per_fcc))

# Now let's combine all of the multipolygon geometry data per ZCTA by county
county_geometries <- broadband_spatial_nj %>% 
  group_by(county) %>% 
  summarise(geometry = st_union(geometry))