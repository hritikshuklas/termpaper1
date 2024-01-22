#### Preamble ####
# Purpose: Simulates... [...UPDATE THIS...]
# Author: Rohan Alexander [...UPDATE THIS...]
# Date: 11 February 2023 [...UPDATE THIS...]
# Contact: rohan.alexander@utoronto.ca [...UPDATE THIS...]
# License: MIT
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? [...UPDATE THIS...]


#### Workspace setup ####
library(tidyverse)
# [...UPDATE THIS...]

#### Simulate data ####
# Each crime rate is listed as crimes recorded per 100,000 people
num_neighbourhoods <- 158
simulated_data <- 
  data.frame(
    neighbourhood_id = c(1:num_neighbourhoods),
    population = runif(
      n = num_neighbourhoods,
      max = 15000,
      min = 9000
    ) |> floor(),
    thefts = rnorm(
      n = num_neighbourhoods,
      mean = 300,
      sd = 20
    ) |> floor(),
    assaults = rnorm(
      n = num_neighbourhoods,
      mean = 210,
      sd = 25
    ) |> floor(),
    homicides = rnorm(
      n = num_neighbourhoods,
      mean = 50,
      sd = 15
    ) |> floor(),
    theft_rate = rnorm(
      n = num_neighbourhoods,
      mean = 30,
      sd = 9
    ),
    assault_rate = rnorm(
      n = num_neighbourhoods,
      mean = 40,
      sd = 8
    ),
    homicide_rate = rnorm(
      n = num_neighbourhoods,
      mean = 15,
      sd =4
    )
  )

# Returns TRUE if the number of rows in dataframe equals n, else FALSE
assert_row_count <- function(df, n){
  return(nrow(df) == n)
}

# Returns TRUE if the number of columns in dataframe equals n, else FALSE
assert_col_count <- function(df, n){
  return(ncol(df) == n)
}

# Check if no entry contains NULL
# Returns TRUE  if no entries are NULL
#         FALSE if at least one entry is NULL
assert_no_null_entries <- function(df){
  return(!any(is.null(df)))
}

# Check if no entry contains NA (also checks for NaN)
# Returns TRUE  if no entries are NA (and NaN)
#         FALSE if at least one entry is NA (or NaN)
assert_no_na_entries <- function(df){
  return(!any(is.na(df)))
}

test_data_valid <- function(df, nrows, ncols){
  return(assert_row_count(df, nrows) && 
           assert_col_count(df, ncols) &&
           assert_no_na_entries(df) &&
           assert_no_null_entries(df))
}

# Ref: https://www.statology.org/r-loop-through-data-frame-columns/
# Check if all entries in given columns are positive integers
# Returns TRUE  if all entries are positive integers
#         FALSE otherwise
test_positive_integers <- function(cols){
  check <- sapply(unlist(cols), FUN=function(x){
    return(is.numeric(x) && x>0 && x%%1==0)
  })
  return(all(check))
}

# Check if all entries in given columns are positive decimals
# Returns TRUE  if all entries are positive decimals
#         FALSE otherwise
test_positive_decimals <- function(cols){
  check <- sapply(unlist(cols), FUN=function(x){
    return(is.numeric(x) && x>0)
    })
  return(all(check))
}

# Check if all entries in given columns are strings
# Returns TRUE  if all entries are positive decimals
#         FALSE otherwise
test_positive_decimals <- function(cols){
  check <- sapply(unlist(cols), FUN=function(x){
    return(is.numeric(x) && x>0)
  })
  return(all(check))
}

### TESTING THE SIMULATED DATA
test_data_valid(simulated_data, 158, 8)
test_positive_integers(select(simulated_data, neighbourhood_id, population, thefts, assaults, homicides))
test_positive_decimals(select(simulated_data, theft_rate, assault_rate, homicide_rate))
