# Intro ----
# 2021 Week 20 exploring broadband internet disparities in the US

# Setup ----
library(tidyverse)
library(tidytuesdayR)

# Data ----
# Load the larger zip code data from the tidytuesdayR package
tuesdata <- tidytuesdayR::tt_load("2021-05-11")
broadband <- tuesdata$broadband

# Explore data
glimpse(broadband)

# Data contains five columns for the state (two letter code), county ID (double), county name (full name), broadband per fcc (percent of people per county with access to broadband at the end of 2017), and broadband usage (percent of people per county that actually use the broadband internet)

# Clean up column names ----
broadband <- broadband %>% 
  clean_names()

# It looks like there are some states with missing data
# Summary plot to visualize the distribution of missing data across states
broadband %>% 
  mutate(broadband_availability_per_fcc = str_replace(broadband_availability_per_fcc, 
                                                      "-", "NA")) %>% 
  filter(broadband_availability_per_fcc == "NA") %>% 
  group_by(st) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(reorder(st, -n), n)) +
  geom_bar(stat = "identity")
  
# Add more to plot 

# Boxplot ----
# Explore the distribution of broadband by state
broadband %>% 
  clean_names() %>% 
  filter(grepl("A[A-Z]", st)) %>% View()
  ggplot(aes(st, broadband_availability_per_fcc, group = st)) +
  geom_boxplot()
  scale_y_continuous(n.breaks = 4)
