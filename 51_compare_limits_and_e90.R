library(tidytable)
library(ggplot2)



load(here::here("data", "R_data_files", "all_potw_permits.Rda"))

icis_permits2 <- icis_permits2 %>%
  select(EXTERNAL_PERMIT_NMBR, VERSION_NMBR, design_flow_round)


read_dmr_data <- function(dmr_year) {
  
  print(dmr_year)
  
  dmr_file <- paste0( "dmr_data_", dmr_year,".Rda")
  
  load(here::here("data", "R_data_files", dmr_file))
  
  dmr_data <- dmr_data %>%
    filter(PERM_FEATURE_TYPE_CODE == "EXO", LIMIT_TYPE_CODE == "ENF",
           MONITORING_LOCATION_CODE %in% c("1", "2", "EG", "Y"),
           !is.na(LIMIT_VALUE_NMBR)) %>%
    filter(!(is.na(DMR_VALUE_NMBR) & is.na(NODI_CODE))) %>%
    select(EXTERNAL_PERMIT_NMBR, VERSION_NMBR, PERM_FEATURE_NMBR, MONITORING_LOCATION_CODE, LIMIT_VALUE_TYPE_CODE,
           LIMIT_VALUE_ID, LIMIT_VALUE_NMBR, DMR_VALUE_NMBR, 
           PARAMETER_CODE, PARAMETER_DESC, STATISTICAL_BASE_CODE, LIMIT_VALUE_QUALIFIER_CODE,
           VIOLATION_CODE, NODI_CODE,
           monitoring_period_end_date2) %>%
    left_join(icis_permits2) %>%
    filter(!is.na(design_flow_round)) %>%
    pivot_wider(names_from = VIOLATION_CODE, values_from = VIOLATION_CODE) 
  
  inv_gc()
  
  
  dmr_data_counts <- dmr_data %>%
    group_by(EXTERNAL_PERMIT_NMBR) %>%
    summarize(num_limits = n(),
              num_e90 = sum(E90 == "E90", na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(e90_rate = num_e90/num_limits,
           dmr_year = dmr_year)
  
}

dmr_years <- as.character(2018:2023)

dmr_e90_count_data <- purrr::map(dmr_years, read_dmr_data)

dmr_data_counts_by_flow <- dmr_e90_count_data %>%
  bind_rows() %>%
  left_join(icis_permits2) %>%
  filter(design_flow_round < 10) %>%
  group_by(design_flow_round) %>%
  summarize(mean_num_limits = mean(num_limits),
            mean_e90 = mean(num_e90),
            mean_e90_rate = mean(e90_rate),
            num_in_bin = n())

ggplot(dmr_data_counts_by_flow %>% filter(design_flow_round <= 2), 
       aes(x = design_flow_round, y = mean_num_limits, size = num_in_bin)) +
  geom_point() +
  #labs(title = "Mean Number of Enforceable Limits by Design Flow") +
  ylab("Mean Number Limits") + xlab("Design Flow (rounded to single decimal)") +
  theme_tina

ggsave("num_limits_by_design_flow_dots.png", path = here::here("figs"),
       h = 8.5, w = 11, units = "in", bg = "white")  

ggplot(dmr_data_counts_by_flow %>% filter(design_flow_round <= 2), 
       aes(x = design_flow_round, y = mean_e90, size = num_in_bin)) +
  geom_point() +
  #labs(title = "Mean Number of Effluent Violations by Design Flow") +
  ylab("Mean Violations") + xlab("Design Flow (rounded to single decimal)") +
  theme_tina

ggsave("e90_by_design_flow_dots.png", path = here::here("figs"),
       h = 8.5, w = 11, units = "in", bg = "white")  

ggplot(dmr_data_counts_by_flow %>% filter(design_flow_round <= 2), 
       aes(x = design_flow_round, y = mean_e90_rate, size = num_in_bin)) +
  geom_point() +
  #labs(title = "Mean Effluent Violation Rate by Design Flow") +
  ylab("Mean Exceedence Rate") + xlab("Design Flow (rounded to single decimal)") +
  theme_tina

ggsave("e90_rate_by_design_flow_dots.png", path = here::here("figs"),
       h = 8.5, w = 11, units = "in", bg = "white")  
