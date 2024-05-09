##  This script extracts flow DMRs

library(readr)
library(dplyr)
library(sqldf)
library(magrittr)
library(tidylog) #wrapper for tidy that gives information on each output
library(tidytable)

# Create temporary folder ----

dir_create_if_not_exist <- function(dir_name, ...) {
  
  
  if (!dir.exists(here::here(dir_name, ...))) {dir.create(here::here(dir_name, ...))}
}

dir_create_if_not_exist("temp")


# Extract flow DMRS ----

# Create a function that extracts rows with Flow DMRs 
get_flow <- function(year){
  
  ## Load DMR data
  load(here::here("data", "R_data_files", paste0("dmr_data_", year, ".Rda")))
  
  colnames(dmr_data) <- tolower(colnames(dmr_data))
  
  dmr_data$date <- as.Date(dmr_data$monitoring_period_end_date, "%m/%d/%Y")
  
  
  ## Select flow parameters
  flow_param_desc <- c("Flow, in conduit or thru treatment plant", "Flow", "Flow, total")
  flow_param_codes <- c("50050", "82220", "74076")
  dmr_data <- dplyr::filter(dmr_data, parameter_desc %in% flow_param_desc | 
                         parameter_code %in% flow_param_codes)
  
  ## Restrict to daily maximums, monthly averages, external outfalls, and effluent gross
  dmr_data <- dplyr::filter(dmr_data, statistical_base_code %in% c("DD", "MK"))
  dmr_data <- dplyr::filter(dmr_data, monitoring_location_code %in% c("1", "EG", "Y") & 
                         perm_feature_type_code == "EXO")
  
  ## Save as a temporary file
  save(dmr_data, file = here::here("temp", paste0("flow_dmrs_", year, ".Rda")))
}

# Extract rows with flow DMRS 

## Create a list of dmr data files
dmr_years <- as.character(2018:2023)

purrr::walk(dmr_years, get_flow)


# Aggregate all years ----
load(here::here("temp","flow_dmrs_2018.Rda"))
flow_dmrs <- dmr_data
for (year in 2019:2023){
  load(here::here("temp",paste0("flow_dmrs_", year, ".Rda")))
  flow_dmrs <- rbind(flow_dmrs, dmr_data)
}

save(flow_dmrs, file = here::here("data", "R_data_files", "flow_dmrs.Rda"))



# Magnitude function ----
#### Create a function that determines magnitude for the flow correction factors
magnitude <- function (v) {
  
  if (is.na(v)){
    m <- NA
  }
  
  if (!is.na(v) & v == 0) {
    m <- 0
  }
  
  for (i in 1:14){
    if (!is.na(v) & v >= 10^-i & v < 10^(-i+1)){
      m <- 10^-i
    }
  }
  
  for (j in 1:10){
    if (!is.na(v) & v < 10^(j+1) & v >= 10^j){
      m <- 10^j
    }
  }
  
  if(!is.na(v) & v < 10 & v >= 1){
    m <- 1
  }
  
  m
}



# Perform flow correction ----

# Generate a year variable
flow_dmrs$year = format(flow_dmrs$date, "%Y")

# QC: check if dmr values vary within dmr_value_id
#flow_dmrs <- flow_dmrs %>%
# group_by(dmr_value_id) %>% 
#mutate(same = n_distinct(dmr_value_standard_units) == 1) %>%
# ungroup

#if (n_distinct(flow_dmrs$same) > 1){
#  break
#}

#flow_dmrs <- select(flow_dmrs, -same)

# Drop extra rows for violations
flow_dmrs <- flow_dmrs %>% 
  group_by(dmr_value_id) %>% 
  slice(1) %>%
  ungroup()

# Create flow variable

flow_dmrs <- flow_dmrs %>%
  mutate(flow = replace(dmr_value_standard_units, nodi_code %in% list("7", "B", "C", "Q", "F", "Y") & is.na(dmr_value_standard_units), 0))

# Drop missing or negative flow values 
flow_dmrs <- flow_dmrs %>%
  filter(!is.na(flow) & flow >= 0)

## Type 1 flow correction ----

# Correct flow following EPA's Technical Users Background Document for the Discharge Monitoring Report (DMR) Pollutant Loading Tool - Section 3.1.2 (https://echo.epa.gov/system/files/Technical_Users_Background_Doc.pdf) 

### Step a: Create a field that identifies the magnitude of each flow

flow_dmrs <- flow_dmrs %>%
  rowwise() %>%
  mutate(magnitude_of_flow = magnitude(flow)) 

### Step b: group flow magnitudes by unique external_permit_nmbr, perm_feature_nmbr, monitoring_location, and limit_set_designator
### Step c: find minimum flow magnitude that is >=1000
### Step d: find maximum flow magnitude that is <1000


magflowlt1000 <- flow_dmrs %>% 
  subset(magnitude_of_flow < 1000) %>%
  group_by(external_permit_nmbr, perm_feature_nmbr, monitoring_location_code, limit_set_designator) %>%
  mutate(max_magnitude = max(magnitude_of_flow)) %>%
  ungroup() %>%
  select(dmr_value_id, max_magnitude)

magflowgt1000 <- flow_dmrs %>%
  subset(magnitude_of_flow >= 1000) %>%
  group_by(external_permit_nmbr, perm_feature_nmbr, monitoring_location_code, limit_set_designator) %>%
  mutate(min_magnitude = min(magnitude_of_flow)) %>%
  ungroup() %>%
  select(dmr_value_id, min_magnitude)


flow <- left_join(flow_dmrs, magflowlt1000, by="dmr_value_id")
flow <- left_join(flow, magflowgt1000, by="dmr_value_id")

## Carry forward and backward the values of min_magnitude and max_magnitude
flow <- flow %>%
  group_by(external_permit_nmbr, perm_feature_nmbr, monitoring_location_code, limit_set_designator) %>%
  tidyr::fill(max_magnitude, .direction = "downup") %>%
  tidyr::fill(min_magnitude, .direction = "downup") %>%
  ungroup()

## Calculate flow correction factor
flow <- flow %>%
  mutate(correction_factor = case_when(max_magnitude == 0 | min_magnitude == 0 ~ NA, .default = min_magnitude/max_magnitude)) %>%
  mutate(flow_correct = case_when(correction_factor >= 10^3 & flow >= 1000 & flow < 5000 & !is.na(correction_factor) ~ flow/correction_factor, 
                                  correction_factor >= 10 & flow >= 5000 & !is.na(correction_factor) ~ flow/correction_factor,
                                  .default = flow))

## Type 2 flow correction (using actual flow and design flow) ----

## Load in ICIS_PERMITS data set for type 2 flow corrections  
icis_permits <- fread(here::here("data", "csv_files", "ICIS_PERMITS.csv"))
colnames(icis_permits) <- tolower(colnames(icis_permits))
icis_permits <- icis_permits %>%
  rename_with(tolower) %>%
  select(external_permit_nmbr, version_nmbr, total_design_flow_nmbr, 
         actual_average_flow_nmbr, major_minor_status_flag, facility_type_indicator)


## Note, total design flow and actual flow are not constant across permit versions
## Determine magnitude of flow for flow correction
# Use actual average flow but if actual average flow is not reported use design flow
icis_permits <- icis_permits %>%
  group_by(external_permit_nmbr, version_nmbr) %>%
  mutate(magnitude_of_flow2 = case_when(is.na(actual_average_flow_nmbr) ~ magnitude(total_design_flow_nmbr),
                                        .default = magnitude(actual_average_flow_nmbr))) %>%
  ungroup()

## Merge icis_permits with flow data set
flow_icis <- inner_join(flow, icis_permits, by=c("external_permit_nmbr", "version_nmbr"))

flow_icis <- flow_icis %>%
  rowwise() %>%
  mutate(correction_factor2 = case_when(magnitude_of_flow2 == 0 | magnitude_of_flow == 0 ~ NA, .default = magnitude_of_flow2/magnitude_of_flow)) %>%
  mutate(flow_correct = replace(flow_correct, is.na(correction_factor) & correction_factor2 <= 10^-3 & flow_correct >= 1000 & flow_correct < 5000 & !is.na(correction_factor2), flow_correct/correction_factor2)) %>%
  mutate(flow_correct = replace(flow_correct, is.na(correction_factor) & correction_factor2 <= 10^-1 & flow_correct >= 5000 & !is.na(correction_factor2), flow_correct/correction_factor2))

## Type 3 flow correction ----
# (if > 5000 and not identified by type 1 or type 2 correction, then assume it was entered as GPD an divide by 1000000)
# Note, some corrected flow values ended up being larger than the reported flow values during the type 2 correction.
# This occurred because of very small correction_factor2. I'll change this back to the original values.
flow_icis <- flow_icis %>%
  rowwise() %>%
  mutate(flow_correct = replace(flow_correct, flow_correct > 5000 & flow <= 5000, flow)) %>%
  mutate(flow_correct = replace(flow_correct, flow_correct > 5000, flow_correct/1000000))

## Save
save(flow_icis, file = here::here("data", "R_data_files", "flow.Rda"))







