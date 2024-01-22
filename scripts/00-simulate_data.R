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
    theft_rate = rnorm(
      n = num_neighbourhoods,
      mean = 30,
      sd = 9
    ),
    autotheft_rate = rnorm(
      n = num_neighbourhoods,
      mean = 50,
      sd = 12
    ),
    robbery_rate = rnorm(
      n = num_neighbourhoods,
      mean = 15,
      sd = 3
    ),
    assault_rate = rnorm(
      n = num_neighbourhoods,
      mean = 40,
      sd = 8
    ),
    shooting_rate = rnorm(
      n = num_neighbourhoods,
      mean = 20,
      sd = 4
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

# Check if no entry contains NULL string
# Returns TRUE  if no entries are NULL string
#         FALSE if at least one entry is NULL string
ans <- is.null(simulated_data)
assert_no_null_entries <- any(ans)

# Check if no entry contains NA
# Returns TRUE  if no entries are NA
#         FALSE if at least one entry is NA
ans <- is.na(simulated_data)
assert_no_na_entries <- any(ans)

# Check if no entry contains NaN
# Returns TRUE  if no entries are NaN
#         FALSE if at least one entry is NaN
nan(simulated_data)
assert_no_nan_entries <- any(ans)

# Check if population is a positive integer
# Returns TRUE  if all entries are positive integers
#         FALSE if at least one entry is a non-positive integer
test_positive_integers <- function(col){
  ans <- TRUE
  for (x in col){
      ans <- (ans && is.numeric(x) && x>0 && x%%1==0)
    }
  return(ans)
}
# test_positive_integers(simulated_data$population)

# Ref: https://www.statology.org/r-loop-through-data-frame-columns/
test_positive_decimals <- function(cols){
  check <- sapply(unlist(cols), FUN=function(x){
    return(is.numeric(x) && x>0)
    })
  return(all(check))
}

ans <- test_positive_decimals(select(simulated_data, homicide_rate, assault_rate))
