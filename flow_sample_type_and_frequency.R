library(tidytable)
library(ggplot2)

load(here::here("data", "R_data_files", "all_potw_permits.Rda"))


flow_counts <- icis_permits2 |> filter(design_flow_round_one_decimal <= 2, VERSION_NMBR == 0) |>
  group_by(design_flow_round_one_decimal) |>
  summarize(count_bin = n(),
            count_actual_flow = sum(!is.na(ACTUAL_AVERAGE_FLOW_NMBR)),
            mean_actual_flow = mean(ACTUAL_AVERAGE_FLOW_NMBR, na.rm = TRUE)) |>
  ungroup() |>
  mutate(pct_actual = count_actual_flow/count_bin)

change_actual <- icis_permits2 |> filter(design_flow_round_one_decimal <= 2) |>
  mutate(VERSION_NMBR = if_else(VERSION_NMBR == 0, 10, VERSION_NMBR)) |>
  group_by(EXTERNAL_PERMIT_NMBR) |>
  mutate(change = if_else(ACTUAL_AVERAGE_FLOW_NMBR == lag(ACTUAL_AVERAGE_FLOW_NMBR), 0, 1))

summary(change_actual)

ref_sample_types <- fread(here::here("data", "REF_SAMPLE_TYPE_0.csv"))
ref_freq_analysis <- fread(here::here("data", "REF_FREQUENCY_OF_ANALYSIS.csv"))

# Create a function that extracts rows with Flow DMRs 
get_flow_sample_info <- function(year){
  
  inv_gc()
  
  print(paste(c("processing year", year)))
  
  ## Load DMR data
  load(here::here("data", "R_data_files", paste0("dmr_data_", year, ".Rda")))
  
  
  ## Select flow parameters
  flow_param_desc <- c("Flow, in conduit or thru treatment plant", "Flow", "Flow, total")
  flow_param_codes <- c("50050", "82220", "74076")
  
  dmr_data <- dmr_data %>%
    filter(PARAMETER_DESC %in% flow_param_desc | PARAMETER_CODE %in% flow_param_codes) |>
    filter(STATISTICAL_BASE_CODE %in% c("DD", "MK")) |>
    filter(MONITORING_LOCATION_CODE %in% c("1", "EG", "Y"), PERM_FEATURE_TYPE_CODE == "EXO") |>
    select(EXTERNAL_PERMIT_NMBR, VERSION_NMBR, PERM_FEATURE_NMBR, MONITORING_LOCATION_CODE, LIMIT_VALUE_TYPE_CODE,
           LIMIT_VALUE_STANDARD_UNITS, STANDARD_UNIT_CODE,
           PARAMETER_CODE, PARAMETER_DESC, STATISTICAL_BASE_CODE, LIMIT_VALUE_QUALIFIER_CODE,
           DMR_FREQ_OF_ANALYSIS_CODE, DMR_SAMPLE_TYPE_CODE,
           LIMIT_FREQ_OF_ANALYSIS_CODE, LIMIT_SAMPLE_TYPE_CODE,
           monitoring_period_end_date2) %>%
    distinct() %>%
    left_join(icis_permits2) %>%
    filter(!is.na(design_flow_round_one_decimal)) |>
    left_join(ref_freq_analysis, by = c("LIMIT_FREQ_OF_ANALYSIS_CODE" = "FREQUENCY_OF_ANALYSIS_CODE")) |>
    rename(limit_freq_analysis = "FREQUENCY_OF_ANALYSIS_DESC") |>
    left_join(ref_freq_analysis, by = c("DMR_FREQ_OF_ANALYSIS_CODE" = "FREQUENCY_OF_ANALYSIS_CODE")) |>
    rename(dmr_freq_analysis = "FREQUENCY_OF_ANALYSIS_DESC") |>
    left_join(ref_sample_types, by = c("LIMIT_SAMPLE_TYPE_CODE" = "SAMPLE_TYPE_CODE")) |>
    rename(limit_sample_type = "SAMPLE_TYPE_DESC") |>
    left_join(ref_sample_types, by = c("DMR_SAMPLE_TYPE_CODE" = "SAMPLE_TYPE_CODE")) |>
    rename(dmr_sample_type = "SAMPLE_TYPE_DESC")
  
  return(dmr_data)
  
  inv_gc()
}

dmr_years <- as.character(2018:2023)

dmr_flow_data <- purrr::map(dmr_years, get_flow_sample_info)

inv_gc()

dmr_flow_data <- dmr_flow_data |>
  bind_rows()

inv_gc()

save(dmr_flow_data, file = here::here("data", "R_data_files", "flow_with_freq_and_sample_type.Rda"), compress = TRUE)



