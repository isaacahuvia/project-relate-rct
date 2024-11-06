## Load packages
library(tidyverse)
library(here)
library(mice)


## Set seed
set.seed(50816)


## Load data
clean_data <- readRDS(here("Data", "Clean Project Relate Data.rds"))


## Impute data
# Get configuration
mice_configuration <- mice(
  clean_data,
  maxit = 0
)

# Don't impute using email, ZIP, or state
predictor_matrix <- mice_configuration$predictorMatrix
predictor_matrix[, c("email_id", "zip", "state")] <- 0

# Impute
imputed_data <- mice(
  clean_data,
  predictor_matrix = predictor_matrix,
  m = 20,
  print = T
)


## Save data
saveRDS(imputed_data, here("Data", "Imputed Project Relate Data.rds"))
