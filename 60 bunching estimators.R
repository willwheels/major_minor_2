library(ggplot2)
library(dplyr)

load(here::here("data", "R_data_files", "all_potw_permits.Rda"))

source(here::here("theme_tina.R"))

top_bin <- 10 ## should be a round number

design_flow_counts_one_decimal <- icis_permits2 %>%
  arrange(EXTERNAL_PERMIT_NMBR, VERSION_NMBR) %>%
  group_by(EXTERNAL_PERMIT_NMBR) %>%             ## keep only most recent permit
  slice(1) %>%                               ##
  ungroup() %>%
  group_by(design_flow_round_one_decimal) %>%
  count()

round_numbers <- seq(0, top_bin, 1)
half_round_numbers <- seq(.5, top_bin - 0.5, 1)
excluded_range_one_decimal <- c(0.9, 1, 1.1)

design_flow_estimation_one_dec <- design_flow_counts_one_decimal |>
  filter(design_flow_round_one_decimal <= top_bin) |>
  mutate(design_flow_est = if_else(design_flow_round_one_decimal %in% round_numbers |
                                     design_flow_round_one_decimal %in% half_round_numbers,
                                   0, design_flow_round_one_decimal),
         round_number_est = if_else(design_flow_round_one_decimal %in% round_numbers, design_flow_round_one_decimal, 0),
         half_number_est = if_else(design_flow_round_one_decimal %in% half_round_numbers, design_flow_round_one_decimal, 0),
         ) |>
  ungroup()

test <- purrr::map(excluded_range_one_decimal, 
                   .f = ~ design_flow_estimation_one_dec |>
                     mutate("dummy_{.x}" := if_else(design_flow_round_one_decimal == .x, 1, 0)) |>
                     select(starts_with("dummy"))
                     
) 


design_flow_estimation_one_dec <- design_flow_estimation_one_dec |>
  bind_cols(test)


design_flow_augment <- design_flow_estimation_one_dec |>
  mutate(across(starts_with("dummy"), ~ .x - .x))

bunching_estimators_one_decimal <- function(poly_value) {

  design_flow_regress <- lm(n ~ poly(design_flow_round_one_decimal, poly_value) 
                            + poly(round_number_est, poly_value)
                            + poly(half_number_est, poly_value) 
                            + dummy_0.9 + dummy_1 + dummy_1.1,
                            data = design_flow_estimation_one_dec)
  
  tidy_results <- broom::tidy(design_flow_regress) |>
    mutate(num_polynomial = poly_value)
  
  tidy_augment <- broom::augment(design_flow_regress, newdata = design_flow_augment) |>
    mutate(num_polynomial = poly_value)
  
  return(list(tidy_results, tidy_augment))
  
}

poly_values <- seq(1:9)

all_results <- purrr::map(poly_values, bunching_estimators_one_decimal)   

all_tidy_results <- purrr::map(all_results, ~.x[[1]]) |>
  bind_rows()

all_augment_results <- purrr::map(all_results, ~.x[[2]]) |>
  bind_rows()

all_rmse <- all_augment_results |>
  group_by(num_polynomial) |>
  mutate(sq_error = .resid*.resid) |>
  summarise(sum_error = sum(sq_error), num = n()) |>
  mutate(rmse = sqrt(sum_error/num))

p1 <- ggplot(design_flow_counts_one_decimal %>% 
               filter(design_flow_round_one_decimal <= top_bin, design_flow_round_one_decimal > 0),
       aes(x = design_flow_round_one_decimal, y = n)) +
  geom_col() +
  annotate("rect", xmin = .7, xmax = 1.2, ymin = 250, ymax = 450, alpha = .2, color = "red") +
  annotate("text", x = 1.5, y = 1500, label = "1 MGD") +
  geom_vline(xintercept = 1, linetype = 2) +
  labs(title = "Histogram of POTW Flows with Bunching Predictions") +
  xlab("Design Flow (rounded to single decimal)") + ylab("Count") +
  theme_minimal()

p1 +
  geom_line(data = all_augment_results |> filter(num_polynomial == 8),
            aes(x = design_flow_round_one_decimal, y = .fitted, group = num_polynomial), color = "blue")
  
p1 + 
  geom_line(data = all_augment_results |> filter(num_polynomial == 8),
            aes(x = design_flow_round_one_decimal, y = .fitted, group = num_polynomial), color = "blue") +
  theme_tina

###########################  

design_flow_counts_two_decimals <- icis_permits2 %>%
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

design_flow_estimation_two_dec <- design_flow_counts_two_decimals |>
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
                                       .f = ~ design_flow_estimation_two_dec |>
                                         mutate("dummy_{.x}" := if_else(design_flow_round_two_decimals == .x, 1, 0)) |>
                                         select(starts_with("dummy"))
                                       
)

design_flow_estimation_two_dec <- design_flow_estimation_two_dec |>
  bind_cols(excluded_dummies_two_dec) 

design_flow_augment_two_dec <- design_flow_estimation_two_dec |>
  mutate(across(starts_with("dummy"), ~ .x - .x)) 

bunching_estimators_two_decimals <- function(poly_value) {
  
  
  design_flow_regress <- lm(n ~ poly(design_flow_round_two_decimals, poly_value) 
                            + poly(round_number_est, poly_value)
                            + poly(half_number_est, poly_value) 
                            + poly(quarter_number_est, poly_value)
                            + poly(tenth_number_est, poly_value)
                            + dummy_0.96 + dummy_0.96 + dummy_0.97 + dummy_0.98 + dummy_0.99
                            + dummy_1 + dummy_1.01 + dummy_1.02 + dummy_1.03 + dummy_1.04 + dummy_1.05,
                            data = design_flow_estimation_two_dec)
  
  tidy_results <- broom::tidy(design_flow_regress) |>
    mutate(num_polynomial = poly_value)
  
  tidy_augment <- broom::augment(design_flow_regress, newdata = design_flow_augment_two_dec) |>
    mutate(num_polynomial = poly_value)
  
  return(list(tidy_results, tidy_augment))
  
}

poly_values <- seq(2:9)

all_results_two_dec <- purrr::map(poly_values, bunching_estimators_two_decimals)   

all_tidy_results_two_dec <- purrr::map(all_results_two_dec, ~.x[[1]]) |>
  bind_rows()

all_augment_results_two_dec <- purrr::map(all_results_two_dec, ~.x[[2]]) |>
  bind_rows()

all_rmse_two_dec <- all_augment_results_two_dec |>
  group_by(num_polynomial) |>
  mutate(sq_error = .resid*.resid) |>
  summarise(sum_error = sum(sq_error), num = n()) |>
  mutate(rmse = sqrt(sum_error/num))


p2 <- ggplot(design_flow_counts_two_decimals
             %>% filter(design_flow_round_two_decimals <= top_bin, design_flow_round_two_decimals > 0),
             aes(x = design_flow_round_two_decimals, y = n)) +
  geom_col() +
  #annotate("rect", xmin = .7, xmax = 1.2, ymin = 250, ymax = 450, alpha = .2, color = "red") +
  annotate("text", x = 1.7, y = 1000, label = "1 MGD") +
  geom_vline(xintercept = 1, linetype = 2) +
  labs(title = "Histogram of POTW Flows") +
  xlab("Design Flow (rounded to two decimals)") + ylab("Count") +
  theme_minimal()

p2 +
  geom_line(data = all_augment_results_two_dec |> filter(num_polynomial > 2),
            aes(x = design_flow_round_two_decimals, y = .fitted, 
                group = num_polynomial, color = num_polynomial))

p3 <- ggplot(design_flow_counts_two_decimals
             %>% filter(design_flow_round_two_decimals <= 3, design_flow_round_two_decimals > 0),
             aes(x = design_flow_round_two_decimals, y = n)) +
  geom_col() +
  #annotate("rect", xmin = .7, xmax = 1.2, ymin = 250, ymax = 450, alpha = .2, color = "red") +
  annotate("text", x = 1.7, y = 1000, label = "1 MGD") +
  geom_vline(xintercept = 1, linetype = 2) +
  labs(title = "Histogram of POTW Flows") +
  xlab("Design Flow (rounded to two decimals)") + ylab("Count") +
  theme_minimal()

p3 +
  geom_line(data = all_augment_results_two_dec |> filter(num_polynomial %in% c(5,6,7,8), design_flow_round_two_decimals <= top_bin/3),
            aes(x = design_flow_round_two_decimals, y = .fitted, 
                group = num_polynomial, color = num_polynomial)) +
  theme(legend.position = "none") +
  theme_tina


ggsave("design_flow_hist_with_bunching.png", path = here::here("figs"),
       h = 8.5, w = 11, units = "in", bg = "white")

all_augment_results_two_dec_just_excluded <- all_augment_results_two_dec |>
  filter(design_flow_round_two_decimals %in% excluded_range)

rmse_just_excluded <- all_augment_results_two_dec_just_excluded |>
  group_by(num_polynomial) |>
  mutate(sq_error = .resid*.resid) |>
  summarise(sum_error = sum(sq_error), num = n()) |>
  mutate(rmse = sqrt(sum_error/num))





#### do logs look better?
test <- design_flow_counts_two_decimals |> 
  filter(design_flow_round_two_decimals <= top_bin, design_flow_round_two_decimals > 0) |>
  mutate(ln_design_flow_round_two_decimals = log(design_flow_round_two_decimals))

ggplot(test, aes(x = ln_design_flow_round_two_decimals, y = n)) +
  geom_col(width = .05) +
  geom_vline(xintercept = log(1), linetype = 2) +
  theme_minimal()
