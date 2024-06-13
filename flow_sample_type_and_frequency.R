library(tidytable)

load(here::here("data", "R_data_files", "all_potw_permits.Rda"))

icis_permits2 <- icis_permits2 %>%
  select(EXTERNAL_PERMIT_NMBR, VERSION_NMBR, design_flow_round_one_decimal)

ref_sample_types <- fread(here::here("data", "REF_SAMPLE_TYPE_0.csv"))
ref_freq_analysis <- fread(here::here("data", "REF_FREQUENCY_OF_ANALYSIS.csv"))

# Create a function that extracts rows with Flow DMRs 
get_flow_sample_info <- function(year){
  
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
  
}

test_flow_data <- get_flow_sample_info("2021")
