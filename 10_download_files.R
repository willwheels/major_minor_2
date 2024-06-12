library(tidytable)
library(ggplot2)

## change if the downloads are timing out; default is 60
options(timeout=300)
#options(timeout=1000000) ##For Tina's PC

if(!file.exists(here::here("data", "csv_files", "ICIS_PERMITS.csv"))) {
  echo_data_url <- "https://echo.epa.gov/files/echodownloads/npdes_downloads.zip"
  
  temp <- tempfile()
  download.file(echo_data_url, temp)
  unzip(temp, file = "ICIS_PERMITS.csv", exdir = here::here("data", "csv_files"))
  unzip(temp, file = "NPDES_PERM_COMPONENTS.csv", exdir = here::here("data", "csv_files"))
  unzip(temp, file = "NPDES_QNCR_HISTORY.csv", exdir = here::here("data", "csv_files"))
  unzip(temp, file = "NPDES_INSPECTIONS.csv", exdir = here::here("data", "csv_files"))
  
  unlink(temp)
}


icis_permits <- fread(here::here("data", "csv_files", "ICIS_PERMITS.csv"))


perm_components_potw <- fread(here::here("data", "csv_files", "NPDES_PERM_COMPONENTS.csv")) %>%
  select(-COMPONENT_TYPE_CODE) %>%
  filter(COMPONENT_TYPE_DESC == "POTW")


perm_components_pretreat <- fread(here::here("data", "csv_files", "NPDES_PERM_COMPONENTS.csv")) %>%
  select(-COMPONENT_TYPE_CODE) %>%
  filter(COMPONENT_TYPE_DESC == "Pretreatment") %>%
  mutate(pretreat_flag = 1) %>%
  select(-COMPONENT_TYPE_DESC)


if(!file.exists(here::here("data", "csv_files", "NPDES_LIMITS.csv"))) {
  echo_data_url <- "https://echo.epa.gov/files/echodownloads/npdes_limits.zip"
  
  temp <- tempfile()
  download.file(echo_data_url, temp)
  unzip(temp, file = "NPDES_LIMITS.csv", exdir = here::here("data", "csv_files"))
  
  
  unlink(temp)
}


npdes_limits <- fread(here::here("data", "csv_files", "NPDES_LIMITS.csv"), 
                      select = c("EXTERNAL_PERMIT_NMBR", "VERSION_NMBR", "LIMIT_TYPE_CODE", "ALL_MONTHS_LIMIT"))

npdes_limits <- npdes_limits %>%
  filter(LIMIT_TYPE_CODE == "ENF") %>%
  mutate(num_months = stringr::str_count(ALL_MONTHS_LIMIT, "JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP|OCT|NOV|DEC")) %>%
  group_by(EXTERNAL_PERMIT_NMBR, VERSION_NMBR) %>%
  summarise(num_limits_per_year = sum(num_months))

summary(npdes_limits)

            
icis_permits2 <- icis_permits %>%
  select(EXTERNAL_PERMIT_NMBR, VERSION_NMBR, FACILITY_TYPE_INDICATOR, PERMIT_TYPE_CODE,
         PERMIT_STATUS_CODE, MAJOR_MINOR_STATUS_FLAG, TOTAL_DESIGN_FLOW_NMBR, 
         ACTUAL_AVERAGE_FLOW_NMBR) %>%
  left_join(perm_components_potw) %>%
  left_join(npdes_limits) %>%
  filter(!(PERMIT_STATUS_CODE %in% c("NON", "TRM"))) %>%
  filter(FACILITY_TYPE_INDICATOR == "POTW" | COMPONENT_TYPE_DESC == "POTW") %>%
  left_join(perm_components_pretreat) %>%
  mutate(pretreat_flag = if_else(is.na(pretreat_flag), 0, 1)) %>%
  arrange(EXTERNAL_PERMIT_NMBR, VERSION_NMBR) %>%
  filter(!is.na(TOTAL_DESIGN_FLOW_NMBR)) %>%
  mutate(flow_over_design = ACTUAL_AVERAGE_FLOW_NMBR - TOTAL_DESIGN_FLOW_NMBR,
         design_flow_round_one_decimal = plyr::round_any(TOTAL_DESIGN_FLOW_NMBR, .1, floor),
         design_flow_round_two_decimals = plyr::round_any(TOTAL_DESIGN_FLOW_NMBR, .01, floor),
         design_flow_round_three_decimals = plyr::round_any(TOTAL_DESIGN_FLOW_NMBR, .001, floor))


summary(icis_permits2)


rm(icis_permits)

inv_gc()
  
qncr <- fread(here::here("data", "csv_files", "NPDES_QNCR_HISTORY.csv"))


qncr <- qncr %>%
  filter(YEARQTR > 20180) %>%
  mutate(year = as.numeric(substr(as.character(YEARQTR), 1, 4)),
         quarter = as.numeric(substr(as.character(YEARQTR), 5, 5)),
         snc_cat_1 = (HLRNC %in% c("D", "E", "S", "T", "X")*1)) %>%
  group_by(NPDES_ID) %>%
  summarise(num_snc = sum(snc_cat_1), num_e90 = sum(NUME90Q))

summary(qncr)


npdes_inspections <- fread(here::here("data", "csv_files", "NPDES_INSPECTIONS.csv"),
                           select = c("NPDES_ID", "ACTIVITY_ID",
                                      "COMP_MONITOR_TYPE_CODE", "COMP_MONITOR_TYPE_DESC", 
                                      "ACTUAL_BEGIN_DATE", "ACTUAL_END_DATE"))


## I could use begin date to try to fill in end date if the latter is missing, which it rarely is

npdes_inspections <- npdes_inspections %>%
  mutate(end_date = lubridate::mdy(ACTUAL_END_DATE)) %>%
  filter(end_date >= lubridate::mdy("01-01-2018")) %>%
  distinct() %>%  ## eliminate multiple entries on the same day
  group_by(NPDES_ID) %>%
  summarise(num_inspections = n(), num_CMS = sum(COMP_MONITOR_TYPE_CODE %in% c("CEI", "SA1", "AU1", "DIA", "CBI",
                                                                               "TX1" ))) #from https://www.epa.gov/sites/default/files/2013-09/documents/npdescms.pdf

summary(npdes_inspections)

icis_permits2 <- icis_permits2 %>%
  left_join(qncr, by = c("EXTERNAL_PERMIT_NMBR" = "NPDES_ID")) %>%
  left_join(npdes_inspections, by = c("EXTERNAL_PERMIT_NMBR" = "NPDES_ID"))



icis_permits_most_recent_summarized <- icis_permits2 %>%
  group_by(EXTERNAL_PERMIT_NMBR) %>%             ## keep only most recent permit
  slice(1) %>%                                   ##
  ungroup() %>%
  mutate(num_snc = replace_na(num_snc, 0)) |>
  mutate(e90_ratio = num_e90/num_limits_per_year) %>%
  filter(TOTAL_DESIGN_FLOW_NMBR <= 10) %>%
  group_by(design_flow_round_one_decimal) %>%
  summarise(mean_snc = mean(num_snc, na.rm = TRUE), 
            mean_e90 = mean(num_e90, na.rm = TRUE),
            mean_num_limits_per_year = mean(num_limits_per_year, na.rm = TRUE),
            mean_inspections = mean(num_inspections, na.rm = TRUE),
            mean_CMS = mean(num_CMS, na.rm = TRUE),
            mean_e90_ratio = mean(e90_ratio, na.rm = TRUE),
            num_in_bin = n()) %>%
  arrange(desc(design_flow_round_one_decimal)) %>%
  ungroup()


save(icis_permits2, file = here::here("data", "R_data_files", "all_potw_permits.Rda"))

save(icis_permits_most_recent_summarized, file = here::here("data", "R_data_files", "all_potw_permits_most_recent.Rda"))


if(!file.exists(here::here("data", "REF_FREQUENCY_OF_ANALYSIS.csv"))) {
  download.file("https://echo.epa.gov/system/files/REF_FREQUENCY_OF_ANALYSIS.csv", 
                destfile = here::here("data", "REF_FREQUENCY_OF_ANALYSIS.csv"))
}
  

if(!file.exists(here::here("data", "REF_SAMPLE_TYPE_0.csv"))) {
  download.file("https://echo.epa.gov/system/files/REF_SAMPLE_TYPE_0.csv", 
                destfile = here::here("data", "REF_SAMPLE_TYPE_0.csv"))
}
