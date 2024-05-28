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
           !is.na(LIMIT_VALUE_NMBR)) %>%
    filter(!(is.na(DMR_VALUE_NMBR) & is.na(NODI_CODE))) %>%
    filter(VIOLATION_CODE == "E90") |>
    select(EXTERNAL_PERMIT_NMBR, VERSION_NMBR, PERM_FEATURE_NMBR, MONITORING_LOCATION_CODE, LIMIT_VALUE_TYPE_CODE,
           LIMIT_VALUE_ID, LIMIT_VALUE_NMBR, DMR_VALUE_NMBR, 
           PARAMETER_CODE, PARAMETER_DESC, STATISTICAL_BASE_CODE, LIMIT_VALUE_QUALIFIER_CODE,
           #VIOLATION_CODE, NODI_CODE,
           monitoring_period_end_date2) %>%
    left_join(icis_permits2) %>%
    filter(!is.na(design_flow_round)) 
  
  inv_gc()
  
  
  dmr_e90_counts <- dmr_data %>%
    group_by(PARAMETER_CODE, PARAMETER_DESC, STATISTICAL_BASE_CODE, design_flow_round) %>%
    count() |>
    ungroup() %>%
    mutate(dmr_year = dmr_year)
  
}

dmr_years <- as.character(2018:2023)

dmr_e90_count_data <- purrr::map(dmr_years, read_dmr_data)

dmr_data_counts_by_flow <- dmr_e90_count_data %>%
  bind_rows() |>
  arrange(desc(n))

dmr_data_counts_by_flow_summed <- dmr_data_counts_by_flow |>
  filter(design_flow_round > 0, design_flow_round <= 2.5) |>
  group_by(PARAMETER_CODE, PARAMETER_DESC, design_flow_round) |>
  summarise(total_viols = sum(n)) |>
  ungroup() |>
  arrange(design_flow_round, desc(total_viols)) |>
  group_by(design_flow_round) |>
  slice_head(n = 10) |>
  ungroup() 
