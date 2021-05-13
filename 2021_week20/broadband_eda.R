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
broadband_zip <- tuesdata$broadband_zip

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
broadband_nomissing <- broadband %>% 
  mutate(broadband_availability_per_fcc = str_replace(broadband_availability_per_fcc, 
                                                      "-", "NA")) %>% 
  filter(broadband_availability_per_fcc != "NA")

# Explore distribution of broadband access across states
broadband_nomissing %>% 
  ggplot(aes(broadband_availability_per_fcc, st, group = st)) +
  geom_density_ridges(scale = 2) +
  scale_x_discrete(breaks = c("0.00", "0.25", "0.50", "0.75", "1.00"), 
                   expand = c(0, 0))

# It's hard to analyze all of these states at once in a single plot
# So, I'll look more deeply at the states with the highest and lowest median broadband availability, as well as the state with the widest distribution across its counties

# Generate median broadband availability across all counties within each state
broadband_avail_summary <- broadband_nomissing %>% 
  group_by(st) %>% 
  summarize(median_availability = median(as.numeric(broadband_availability_per_fcc)))

# What state has the highest median availability?
broadband_avail_summary %>% 
  arrange(desc(median_availability))

# There are several states with almost 100% broadband availability- CT, NJ, RI, WA
# I'll choose my home state of NJ 

# What state has the losest median availability?
broadband_avail_summary %>% 
  arrange(median_availability)

# AR has the lowest median availability

# Explore NJ data further ----
# Here, we'll use the tigris package to grab shape files from the TIGER/Line repository from the US Census Bureau
# First, create a basic plot of the state of NJ using the broadband zip data
# We'll use the counties() function to download a shape file from the US Census Bureau
counties_nj <- counties(state = "NJ")
ggplot(counties_nj) +
  geom_sf()

# Get the zctas (Zip Code Tabulated Areas) for NJ
# Note, this won't take as long as the option "cb = FALSE"
zctas_nj <- zctas(cb = TRUE, state = "NJ")

# Find all ZIP codes for NJ
zipcodes_nj <- search_state("NJ")

# Merge the two files into a single data frame, matching "zipcode" with "GEOID10"
joined_data <- left_join(zipcodes_nj, zctas_nj, by = c("zipcode" = "GEOID10"))

ggplot(zctas_nj) +
  geom_sf()
