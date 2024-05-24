##  This script extracts pollutant DMRs

library(readr)
library(dplyr)
library(sqldf)
library(magrittr)
library(tidylog) #wrapper for tidy that gives information on each output
library(tidytable)
library(udpipe)


# Extract DMRs for five conventional parameters ----

## Parameter codes for five conventional parameters
param_codes <- c("00310", "00300", "00530", "31641", "00400") ## BOD, DO, TSS, Fecal coliform, pH

## Create a function that extracts rows with conventional pollutants' DMRs ----
get_pollutant_dmrs <- function(year){
  
  ## Load DMR data
  load(here::here("data", "R_data_files", paste0("dmr_data_", year, ".Rda")))
  
  colnames(dmr_data) <- tolower(colnames(dmr_data))
  
  dmr_data$date <- as.Date(dmr_data$monitoring_period_end_date, "%m/%d/%Y")
  
  
  ## Select parameters
  dmr_data <- dplyr::filter(dmr_data, parameter_code %in% param_codes)
  
  ## Restrict to daily maximums, monthly averages, external outfalls, and effluent gross
  dmr_data <- dplyr::filter(dmr_data, statistical_base_code %in% c("DD", "MK"))
  dmr_data <- dplyr::filter(dmr_data, monitoring_location_code %in% c("1", "EG", "Y") & 
                              perm_feature_type_code == "EXO")
  
  ## Save as a temporary file
  save(dmr_data, file = here::here("temp", paste0("pollutant_dmrs_", year, ".Rda")))
}


# Apply function to each year
dmr_years <- as.character(2018:2023)
purrr::walk(dmr_years, get_pollutant_dmrs)



# Aggregate all years ----
load(here::here("temp","pollutant_dmrs_2018.Rda"))
pollutant_dmrs <- dmr_data
for (year in 2019:2023){
  load(here::here("temp",paste0("pollutant_dmrs_", year, ".Rda")))
  pollutant_dmrs <- rbind(pollutant_dmrs, dmr_data)
}

save(pollutant_dmrs, file = here::here("temp", "pollutant_dmrs.Rda"))



# Generate a year variable
pollutant_dmrs$year = format(pollutant_dmrs$date, "%Y")


# Drop extra rows for violations
pollutant_dmrs <- pollutant_dmrs %>% 
  group_by(dmr_value_id) %>% 
  slice(1) %>%
  ungroup()


# Determine pollutant amount and concentration ----

## Create a pollutant variable, if NODI code indicates no discharge or a valid no value, then replace pollutant value with 0
pollutant_dmrs <- pollutant_dmrs %>%
  mutate(pollutant_value = replace(dmr_value_standard_units, nodi_code %in% list("7", "B", "C", "Q", "F", "Y") & is.na(dmr_value_standard_units), 0))

## Ask Will about missing pollutant values, they might not be zeroes

## Create variable for pollutant type
pollutant_dmrs <- pollutant_dmrs %>%
  mutate(pollutant_type = tidytable::case_when(parameter_code == "00310" ~ "BOD",
                                               parameter_code == "00300" ~ "DO",
                                               parameter_code == "00530" ~ "TSS",
                                               parameter_code == "31641" ~ "FColiform", 
                                               parameter_code == "00400" ~ "pH"))

## Check for errors
table(pollutant_dmrs$statistical_base_code, pollutant_dmrs$statistical_base_type_code)

## If below detection limit and has missing value, replace with zero. Then remove negative values and winsorize at 99th percentile.
pollutant_dmrs <- pollutant_dmrs %>%
  mutate(pollutant_value = replace(pollutant_value, dmr_value_qualifier_code %in% list("<", "<=") & is.na(pollutant_value), 0)) %>%
  mutate(pollutant_value = replace(pollutant_value, pollutant_value < 0 & !is.na(pollutant_value), NA)) %>%
  group_by(parameter_code, statistical_base_type_code) %>%
  mutate(pollutant_value = replace(pollutant_value, pollutant_value > quantile(pollutant_value, 0.99, na.rm = TRUE) & !is.na(pollutant_value), 
                                   quantile(pollutant_value, 0.99, na.rm = TRUE))) %>%
  ungroup()



## Get data on major/minor status, design flow, POTW status from ICIS_PERMITS
icis_permits <- fread(here::here("data", "csv_files", "ICIS_PERMITS.csv"))
colnames(icis_permits) <- tolower(colnames(icis_permits))
icis_permits <- icis_permits %>%
  rename_with(tolower) %>%
  select(external_permit_nmbr, version_nmbr, total_design_flow_nmbr, permit_status_code,
         actual_average_flow_nmbr, major_minor_status_flag, facility_type_indicator)


pollutant_dmrs <- inner_join(pollutant_dmrs, icis_permits, by=c("external_permit_nmbr", "version_nmbr"))



## Create separate object for each value_type_code
c2 <- pollutant_dmrs %>%
  dplyr:: filter(value_type_code == "C2")

c3 <- pollutant_dmrs %>%
  dplyr:: filter(value_type_code == "C3")

q1 <- pollutant_dmrs %>%
  dplyr:: filter(value_type_code == "Q1")

q2 <- pollutant_dmrs %>%
  dplyr:: filter(value_type_code == "Q2")


# Calculate means and maximums
c2 <- c2 %>%
  mutate(mean_conc = replace(pollutant_value, statistical_base_type_code != "AVG", NA))

c3 <- c3 %>%
  mutate(max_conc = replace(pollutant_value, statistical_base_type_code != "MAX", NA))

q1 <- q1 %>%
  mutate(mean_quant = replace(pollutant_value, statistical_base_type_code != "AVG", NA))

q2 <- q2 %>%
  mutate(max_quant = replace(pollutant_value, statistical_base_type_code != "MAX", NA))


## Save
save(c2, file = here::here("data", "R_data_files", "mean_conc.Rda"))
save(c3, file = here::here("data", "R_data_files", "max_conc.Rda"))
save(q1, file = here::here("data", "R_data_files", "mean_quant.Rda"))
save(q2, file = here::here("data", "R_data_files", "max_quant.Rda"))














