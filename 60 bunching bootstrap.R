## This script calculates bootstrap standard errors for the bunching estimate
library(boot)
library(ggplot2)
library(dplyr)

load(here::here("data", "R_data_files", "all_potw_permits.Rda"))

source(here::here("theme_tina.R"))

top_bin <- 10 ## should be a round number


###########################  





# Function for input into boot function ----
bunching_estimators_two_decimals <- function(data, indices) {
  
  d <- data[indices,]
  
  ## Clean data -- must start on non-grouped data
  d <- d %>%
    arrange(EXTERNAL_PERMIT_NMBR, VERSION_NMBR) %>%
    group_by(EXTERNAL_PERMIT_NMBR) %>%             ## keep only most recent permit
    slice(1) %>%                               ##
    ungroup() %>%
    group_by(design_flow_round_two_decimals) %>%
    count()
  
  round_numbers <- seq(0, top_bin, 1)
  half_round_numbers <- seq(.5, top_bin - 0.5, 1)
  quarter_round_numbers <- seq(.25, top_bin - .25, .5)
  tenth_round_numbers <- seq(0.1, top_bin, .1)
  excluded_range <- seq(0.95, 1.05, by = 0.01)
  
  d <- d |>
    filter(design_flow_round_two_decimals <= top_bin, design_flow_round_two_decimals > .1) |>
    mutate(round_number_est = if_else(design_flow_round_two_decimals %in% round_numbers, 
                                      design_flow_round_two_decimals, 0),
           half_number_est = if_else(design_flow_round_two_decimals %in% half_round_numbers, 
                                     design_flow_round_two_decimals, 0),
           quarter_number_est = if_else(design_flow_round_two_decimals %in% quarter_round_numbers, 
                                        design_flow_round_two_decimals, 0),
           tenth_number_est = if_else(design_flow_round_two_decimals %in% tenth_round_numbers &
                                        !design_flow_round_two_decimals %in% round_numbers &
                                        !design_flow_round_two_decimals %in% half_round_numbers,
                                      design_flow_round_two_decimals, 0)
    ) |>
    ungroup()
  
  excluded_dummies_two_dec <- purrr::map(excluded_range, 
                                         .f = ~ d |>
                                           mutate("dummy_{.x}" := if_else(design_flow_round_two_decimals == .x, 1, 0)) |>
                                           select(starts_with("dummy"))
                                         
  )
  
  d <- d |>
    bind_cols(excluded_dummies_two_dec) 
  
  design_flow_augment_two_dec <- d |>
    mutate(across(starts_with("dummy"), ~ .x - .x)) 
  
  ## Perform calculation
  
  
  design_flow_regress <- lm(n ~ poly(design_flow_round_two_decimals, poly_value) 
                            + poly(round_number_est, poly_value)
                            + poly(half_number_est, poly_value) 
                            + poly(quarter_number_est, poly_value)
                            + poly(tenth_number_est, poly_value)
                            + dummy_0.95 + dummy_0.96 + dummy_0.97 + dummy_0.98 + dummy_0.99
                            + dummy_1 + dummy_1.01 + dummy_1.02 + dummy_1.03 + dummy_1.04 + dummy_1.05,
                            data = d)
  
  
  ## Set excluded range dummies' coefficients to 0
  design_flow_regress$coefficients[which(names(design_flow_regress$coefficients) %in% "dummy_0.95")] <- 0
  design_flow_regress$coefficients[which(names(design_flow_regress$coefficients) %in% "dummy_0.96")] <- 0
  design_flow_regress$coefficients[which(names(design_flow_regress$coefficients) %in% "dummy_0.97")] <- 0
  design_flow_regress$coefficients[which(names(design_flow_regress$coefficients) %in% "dummy_0.98")] <- 0
  design_flow_regress$coefficients[which(names(design_flow_regress$coefficients) %in% "dummy_0.99")] <- 0
  design_flow_regress$coefficients[which(names(design_flow_regress$coefficients) %in% "dummy_1")] <- 0
  design_flow_regress$coefficients[which(names(design_flow_regress$coefficients) %in% "dummy_1.01")] <- 0
  design_flow_regress$coefficients[which(names(design_flow_regress$coefficients) %in% "dummy_1.02")] <- 0
  design_flow_regress$coefficients[which(names(design_flow_regress$coefficients) %in% "dummy_1.03")] <- 0
  design_flow_regress$coefficients[which(names(design_flow_regress$coefficients) %in% "dummy_1.04")] <- 0
  design_flow_regress$coefficients[which(names(design_flow_regress$coefficients) %in% "dummy_1.05")] <- 0
  
  
  # Get bunching estimate (observed - counterfactual)
  observed_b <- sum(subset(d, 
                           design_flow_round_two_decimals >= 0.95 & design_flow_round_two_decimals < 1)$n)
  
  counterfactual_b <- sum(predict(design_flow_regress, newdata = subset(d, 
                     design_flow_round_two_decimals >= 0.95 & design_flow_round_two_decimals < 1)))

  Bhat <- observed_b - counterfactual_b
  
  return(Bhat)
  
}

bootstrap <- boot(data = icis_permits2,
                  statistic = bunching_estimators_two_decimals, R = 1000)



# Bootstrap for 1st to 9th order polynomials ----

poly_values <- seq(1:9)

for (poly_value in poly_values){

bunching_estimators_two_decimals <- function(data, indices) {
  
  d <- data[indices,]
  
  ## Clean data -- must start on non-grouped data
  d <- d %>%
    arrange(EXTERNAL_PERMIT_NMBR, VERSION_NMBR) %>%
    group_by(EXTERNAL_PERMIT_NMBR) %>%             ## keep only most recent permit
    slice(1) %>%                               ##
    ungroup() %>%
    group_by(design_flow_round_two_decimals) %>%
    count()
  
  round_numbers <- seq(0, top_bin, 1)
  half_round_numbers <- seq(.5, top_bin - 0.5, 1)
  quarter_round_numbers <- seq(.25, top_bin - .25, .5)
  tenth_round_numbers <- seq(0.1, top_bin, .1)
  excluded_range <- seq(0.95, 1.05, by = 0.01)
  
  d <- d |>
    filter(design_flow_round_two_decimals <= top_bin, design_flow_round_two_decimals > .1) |>
    mutate(round_number_est = if_else(design_flow_round_two_decimals %in% round_numbers, 
                                      design_flow_round_two_decimals, 0),
           half_number_est = if_else(design_flow_round_two_decimals %in% half_round_numbers, 
                                     design_flow_round_two_decimals, 0),
           quarter_number_est = if_else(design_flow_round_two_decimals %in% quarter_round_numbers, 
                                        design_flow_round_two_decimals, 0),
           tenth_number_est = if_else(design_flow_round_two_decimals %in% tenth_round_numbers &
                                        !design_flow_round_two_decimals %in% round_numbers &
                                        !design_flow_round_two_decimals %in% half_round_numbers,
                                      design_flow_round_two_decimals, 0)
    ) |>
    ungroup()
  
  excluded_dummies_two_dec <- purrr::map(excluded_range, 
                                         .f = ~ d |>
                                           mutate("dummy_{.x}" := if_else(design_flow_round_two_decimals == .x, 1, 0)) |>
                                           select(starts_with("dummy"))
                                         
  )
  
  d <- d |>
    bind_cols(excluded_dummies_two_dec) 
  
  design_flow_augment_two_dec <- d |>
    mutate(across(starts_with("dummy"), ~ .x - .x)) 
  
  ## Perform calculation
  
  
  design_flow_regress <- lm(n ~ poly(design_flow_round_two_decimals, poly_value) 
                            + poly(round_number_est, poly_value)
                            + poly(half_number_est, poly_value) 
                            + poly(quarter_number_est, poly_value)
                            + poly(tenth_number_est, poly_value)
                            + dummy_0.95 + dummy_0.96 + dummy_0.97 + dummy_0.98 + dummy_0.99
                            + dummy_1 + dummy_1.01 + dummy_1.02 + dummy_1.03 + dummy_1.04 + dummy_1.05,
                            data = d)
  
  
  ## Set excluded range dummies' coefficients to 0
  design_flow_regress$coefficients[which(names(design_flow_regress$coefficients) %in% "dummy_0.95")] <- 0
  design_flow_regress$coefficients[which(names(design_flow_regress$coefficients) %in% "dummy_0.96")] <- 0
  design_flow_regress$coefficients[which(names(design_flow_regress$coefficients) %in% "dummy_0.97")] <- 0
  design_flow_regress$coefficients[which(names(design_flow_regress$coefficients) %in% "dummy_0.98")] <- 0
  design_flow_regress$coefficients[which(names(design_flow_regress$coefficients) %in% "dummy_0.99")] <- 0
  design_flow_regress$coefficients[which(names(design_flow_regress$coefficients) %in% "dummy_1")] <- 0
  design_flow_regress$coefficients[which(names(design_flow_regress$coefficients) %in% "dummy_1.01")] <- 0
  design_flow_regress$coefficients[which(names(design_flow_regress$coefficients) %in% "dummy_1.02")] <- 0
  design_flow_regress$coefficients[which(names(design_flow_regress$coefficients) %in% "dummy_1.03")] <- 0
  design_flow_regress$coefficients[which(names(design_flow_regress$coefficients) %in% "dummy_1.04")] <- 0
  design_flow_regress$coefficients[which(names(design_flow_regress$coefficients) %in% "dummy_1.05")] <- 0
  
  
  # Get bunching estimate (observed - counterfactual)
  observed_b <- sum(subset(d, 
                           design_flow_round_two_decimals >= 0.95 & design_flow_round_two_decimals < 1)$n)
  
  counterfactual_b <- sum(predict(design_flow_regress, newdata = subset(d, 
                                                                        design_flow_round_two_decimals >= 0.95 & design_flow_round_two_decimals < 1)))
  
  Bhat <- observed_b - counterfactual_b
  
  return(Bhat)
  
}

assign(paste0("bootstrap_p", as.character(poly_value)), boot(data = icis_permits2,
           statistic = bunching_estimators_two_decimals, R = 500))

}

