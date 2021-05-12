# Intro ----
# 2021 Week 20 exploring broadband internet disparities in the US

# Setup ----
library(tidyverse)
library(tidytuesdayR)
library(janitor)
library(ggtext)

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

# Boxplot ----
# Explore the distribution of broadband by state
broadband %>% 
  clean_names() %>% 
  filter(grepl("A[A-Z]", st)) %>% View()
  ggplot(aes(st, broadband_availability_per_fcc, group = st)) +
  geom_boxplot() +
  scale_y_continuous(expand = c(0, 0))
