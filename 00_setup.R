
dir_create_if_not_exist <- function(dir_name, ...) {
  
  
  if (!dir.exists(here::here(dir_name, ...))) {dir.create(here::here(dir_name, ...))}
}

dir_create_if_not_exist("data")
dir_create_if_not_exist("data", "csv_files")
dir_create_if_not_exist("data", "R_data_files")
dir_create_if_not_exist("figs")
