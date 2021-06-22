# README

Source code and plots for the Tidy Tuesday 2021 Week #20 data set exploring broadband internet disparities in the United States.  The FCC defines the broadband internet standards as 25 Mbps (download) and 3 Mbps (upload).  I first explored the proportion of missing data per county across the United States, then decided to focus on my home state of New Jersey for a more detailed analysis.  Specifically, I compared the availability and usage of broadband internet access at the county level across the state.  Broadband availability and usage are defined as the percentage of people per county (where data are available) that have access to internet and use the internet at the defined download and upload speeds, respectively.  

There are three additional files in this directory:

* `broadband_eda.R`: script detailing my analysis from reading the broadband csv file, importing multipolygon geometry for all counties in New Jersey, and creating the summary figures.  This script also includes an analysis for analyzing additional data for each postal code using the `tigris` and `zipcodeR` packages, which were not included in the broadband data (broadband data were per county, not per postal code).  

* `missing_data_by_state.png`: figure showing the distribution of missing data per county across the United States.

* `broadband_availability_usage_new_jersey.png`: figure showing the broadband internet availability and usage in New Jersey for each county side-by-side.