library(tidytable)

#remotes::install_version("data.table", version = "1.15.0")

options(timeout = 300)
#options(timeout = 1000000) ##For Tina's PC

download_dmr_file <- function(dmr_year) {
  
  
  path = here::here("data", "zip_files")
  
  
  dmr_url <- paste0("https://echo.epa.gov/files/echodownloads/npdes_dmrs_fy",
                    dmr_year,
                    ".zip")
  
  
  download_filename_full <- stringr::str_split_1(dmr_url, "/")
  download_filename_full <- download_filename_full[length(download_filename_full)]
  #filename_no_ext <- stringr::str_split_1(filename_full, "\\.")[1]
  
  destination_file <- here::here(path, download_filename_full)
  
  
  if(!file.exists(destination_file)) {
    print(paste(dmr_year, "downloading"))
    download.file(dmr_url, destfile = destination_file, mode = "wb")}
  else {print(paste(dmr_year, " already downloaded"))}
  
}


fread_dmrs <- function(dmr_year) {
  
  print(paste("process dmr files for",  dmr_year))
  
  dmr_file <- here::here("data", "zip_files", paste0("npdes_dmrs_fy", dmr_year, ".zip"))
  
  unzip(dmr_file, files = paste0("NPDES_DMRS_FY",dmr_year,".csv"), exdir = here::here("data", "csv_files"))
  
  dmr_file <- here::here("data", "csv_files", paste0("NPDES_DMRS_FY", dmr_year, ".csv"))
  
  dmr_data <- tidytable::fread(dmr_file, 
                    select = c(EXTERNAL_PERMIT_NMBR = "character",
                               VERSION_NMBR = "integer",
                               PERM_FEATURE_NMBR = "character",
                               PERM_FEATURE_TYPE_CODE = "character",
                               MONITORING_LOCATION_CODE = "character",
                               PARAMETER_CODE = "character",
                               PARAMETER_DESC = "character", 
                               LIMIT_TYPE_CODE = "character",
                               LIMIT_VALUE_NMBR = "numeric",
                               LIMIT_UNIT_CODE  = "character",
                               LIMIT_VALUE_QUALIFIER_CODE = "character",
                               LIMIT_VALUE_ID = "character",
                               LIMIT_SET_DESIGNATOR = "character",
                               DMR_VALUE_NMBR = "numeric",
                               DMR_UNIT_CODE = "character",
                               DMR_VALUE_QUALIFIER_CODE = "character",
                               DMR_VALUE_STANDARD_UNITS = "numeric",
                               DMR_VALUE_ID = "character",
                               VALUE_TYPE_CODE = "character",
                               NODI_CODE = "character",
                               EXCEEDENCE_PCT = "integer64",
                               STATISTICAL_BASE_CODE = "character",
                               STATISTICAL_BASE_TYPE_CODE = "character",
                               MONITORING_PERIOD_END_DATE = "character",
                               VALUE_RECEIVED_DATE = "character", 
                               DAYS_LATE = "integer",
                               VIOLATION_CODE = "character"
        )
  )
  
  dmr_data <- dmr_data %>%
    mutate(monitoring_period_end_date2 = lubridate::mdy(MONITORING_PERIOD_END_DATE),
           value_received_date2 = lubridate::mdy(VALUE_RECEIVED_DATE)) %>%
    select(-MONITORING_PERIOD_END_DATE, -VALUE_RECEIVED_DATE)
  
  

  
  save(dmr_data, file = here::here("data", "R_data_files", paste0("dmr_data_", dmr_year, ".Rda")))
  
}


dmr_years <- as.character(2018:2023)

purrr::walk(dmr_years, download_dmr_file)

purrr::walk(dmr_years, fread_dmrs)


## Erase zip files

delete_zip_files <- function(dmr_year){
  
  print(paste("delete zip files for",  dmr_year))
  
  dmr_file <- here::here("data", "zip_files", paste0("npdes_dmrs_fy", dmr_year, ".zip"))
  
  if (file.exists(dmr_file)) {
    file.remove(dmr_file)
  }
  
}

purrr::walk(dmr_years, delete_zip_files)

