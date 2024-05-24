## This script cleans the inspections data


# Download the inspections data ----

library(tidytable)
library(tidylog) #wrapper for tidy that gives information on each output
library(reshape2)
library(dplyr)
library(stringr)
library(rdbounds)
library(formattable)

#remotes::install_version("data.table", version = "1.15.0")

options(timeout = 300)
options(timeout = 1000000) ##For Tina's PC


path = here::here("data", "zip_files")

url <- "https://echo.epa.gov/files/echodownloads/npdes_downloads.zip"

destination_file <- here::here(path, "ndpes_downloads")


if(!file.exists(destination_file)) {
  
  print("downloading")
  download.file(url, destfile = destination_file, mode = "wb")
  unzip(destination_file, exdir = here::here("data", "csv_files"))
} else{print("already downloaded")}



# Load inspections data

insp <- tidytable::fread(here::here("data", "csv_files", "NPDES_INSPECTIONS.csv"))
colnames(insp) <- tolower(colnames(insp))


## Create an indicator that equals 1 if a permit has been inspected at least once on a given date.
insp$inspected <- 1

insp <- insp %>%
  select(npdes_id, actual_end_date, inspected) %>%
  mutate(end_date = lubridate::mdy(actual_end_date)) %>%
  filter(end_date >= lubridate::mdy("01-01-2018"))

insp <- insp[!duplicated(insp),] %>%
  mutate(year = substr(end_date,1,4), quarter = substr(quarters(as.Date(end_date)),2, 2)) %>%
  mutate(yearqtr = paste0(year,quarter))

## Collapse to the month level and reshape wide
insp <- insp %>%
  select(inspected, yearqtr, npdes_id) %>%
  distinct() %>%
  melt(id.vars=c("npdes_id","yearqtr")) %>%
  dcast(npdes_id ~ variable + yearqtr) %>%
  mutate_at(vars(starts_with("inspected_")), list(~if_else(is.na(.), 0, .)))


## Reshape back to month
insp <- insp %>%
  reshape(direction = "long", varying = 2:ncol(insp), idvar = "npdes_id", 
          sep = "_", timevar = "yearqtr") %>%
  rename(external_permit_nmbr = npdes_id)


## Determine who wasn't inspected in a given month using ICIS_PERMITS
load(here::here("data", "R_data_files", "all_potw_permits.Rda"))
colnames(icis_permits2) <- tolower(colnames(icis_permits2))

icis_permits <- icis_permits2 %>%
  arrange(external_permit_nmbr, version_nmbr) %>%
  group_by(external_permit_nmbr) %>% 
  slice(1) %>%
  select(external_permit_nmbr, version_nmbr, facility_type_indicator, 
         permit_type_code, permit_status_code, major_minor_status_flag,
         total_design_flow_nmbr, actual_average_flow_nmbr, component_type_desc,
         num_limits_per_year, pretreat_flag, design_flow_round, design_flow_round_smaller,
         design_flow_round_small) %>%
  ungroup()

inspections <- insp %>%
  full_join(icis_permits, by = "external_permit_nmbr") %>% ## full join because want POTWs that did not get inspected
  mutate(inspected = replace_na(inspected, 0))


## Prepare violations -- determine if they had a E90 violation
qncr <- fread(here::here("data", "csv_files", "NPDES_QNCR_HISTORY.csv"))
colnames(qncr) <- tolower(colnames(qncr))

qncr <- qncr %>%
  filter(yearqtr > 20180) %>%
  mutate(snc_cat_1 = (hlrnc %in% c("D", "E", "S", "T", "X")*1)) %>%
  rename(external_permit_nmbr = npdes_id) %>%
  group_by(external_permit_nmbr, yearqtr) %>%
  summarise(num_snc = sum(snc_cat_1), num_e90 = sum(nume90q)) %>%
  ungroup()
  

insp_viol <- inspections %>%
  left_join(qncr, by = c("external_permit_nmbr", "yearqtr")) %>% ## Not a full join bc should already have all POTWs from previous join
  mutate(inspected = replace_na(inspected, 0), num_snc = replace_na(num_snc, 0),
         num_e90 = replace_na(num_e90, 0)) %>%
  mutate(has_snc = (num_snc > 0), has_e90 = (num_e90 > 0), 
         major = (major_minor_status_flag == "M")) 
  


## Calculate RD bounds
## Set set up, outcome is one of the violations variables, running variable is total_design_flow, treatment is inspections
## This is a fuzzy RD

## Keep obs for which all variables are not missing
sample <- subset(insp_viol, (is.na(has_e90) == FALSE | is.na(has_snc) == FALSE)
                 & (is.na(total_design_flow_nmbr) == FALSE & is.na(inspected) == FALSE))

rdbounds_e90 <- rdbounds(y = sample$has_e90, x = sample$total_design_flow_nmbr, c = 1, 
                         treatment = sample$inspected, discrete_x = FALSE,
                         discrete_y = TRUE, bwsx = 0.25)
rdbounds_summary(rdbounds_e90)

rdbounds_nume90 <- rdbounds(y = sample$num_e90, x = sample$total_design_flow_nmbr, c = 1, 
                         treatment = sample$inspected, discrete_x = FALSE,
                         discrete_y = FALSE, bwsx = 0.25, bwy = 0.1, orders = 2)
rdbounds_summary(rdbounds_nume90)

rdbounds_snc <- rdbounds(y = sample$has_snc, x = sample$total_design_flow_nmbr, c = 1, 
                         treatment = sample$inspected, discrete_x = FALSE,
                         discrete_y = TRUE, bwsx = 0.25)
rdbounds_summary(rdbounds_snc)

rdbounds_numsnc <- rdbounds(y = sample$num_snc, x = sample$total_design_flow_nmbr, c = 1, 
                         treatment = sample$inspected, discrete_x = FALSE,
                         discrete_y = TRUE, bwsx = 0.25)
rdbounds_summary(rdbounds_numsnc)



## Create lags
