#### Preamble ####
# Purpose: Clean the raw data extracted from opendatatoronto
# Author: Hritik Shukla
# Date: 22 January 2024
# Contact: hritik.shukla@mail.utoronto.ca
# License: MIT
# Pre-requisites: 01-download_data.R

#### Workspace setup ####
library(tidyverse)
library(dplyr)
library(janitor)

#### Clean data ####
raw_data <- read_csv("inputs/data/unedited_data.csv")

# Remove unused columns, clean column names, remove any NA values
# Converting NA values to 0s
# Ref: https://stackoverflow.com/a/46243006
cleaned_data <- raw_data |>
  janitor::clean_names() |> 
  select(id,area_name, contains(c("assault", "robbery", "homicide", "shooting"))) |>
  # select(-contains(c("_2014", "_2015","_2016","_2017","_2018","_2019"))) |>
  # select(order(colnames(cleaned_data))) |>
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))

#### Save data ####
write_csv(cleaned_data, "outputs/data/cleaned_data.csv")
