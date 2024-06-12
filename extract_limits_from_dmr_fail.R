




dmr_data <- dmr_data %>%
  filter(PERM_FEATURE_TYPE_CODE == "EXO", LIMIT_TYPE_CODE == "ENF",
         MONITORING_LOCATION_CODE %in% c("1", "2", "EG", "Y"),
         !is.na(LIMIT_VALUE_NMBR)) %>%
  select(EXTERNAL_PERMIT_NMBR, VERSION_NMBR, PERM_FEATURE_NMBR, MONITORING_LOCATION_CODE, LIMIT_VALUE_TYPE_CODE,
         LIMIT_VALUE_NMBR, LIMIT_UNIT_CODE, LIMIT_VALUE_STANDARD_UNITS,
         PARAMETER_CODE, PARAMETER_DESC, STATISTICAL_BASE_CODE, LIMIT_VALUE_QUALIFIER_CODE,
         monitoring_period_end_date2) %>%
  distinct() %>%
  left_join(icis_permits2) %>%
  filter(!is.na(design_flow_round))

smol <- head(dmr_data, 100000)

inv_gc()

## consider doing something for months, e.g. seasonal, that don't have limits
dmr_limits <- dmr_data %>%
  filter(PARAMETER_CODE %in% c("00530", "80082", "00610"), STATISTICAL_BASE_CODE == "MK",
         PERM_FEATURE_NMBR == "001") |>
  select(-LIMIT_VALUE_ID, -NODI_CODE, -VIOLATION_CODE) |>
  distinct() %>%
  group_by(EXTERNAL_PERMIT_NMBR, PARAMETER_CODE, MONITORING_LOCATION_CODE, LIMIT_VALUE_NMBR,
           STATISTICAL_BASE_CODE, LIMIT_UNIT_CODE, monitoring_period_end_date2) %>% 
  mutate(num = n())

group_by(design_flow_round, PARAMETER_CODE, LIMIT_UNIT_CODE) %>%
  #group_by(PARAMETER_CODE, STATISTICAL_BASE_CODE, LIMIT_UNIT_CODE) %>%
  summarise(mean_limit = mean(LIMIT_VALUE_NMBR),
            num_in_bin = n()) %>%
  filter(design_flow_round <= 2.0) %>%
  arrange(PARAMETER_CODE, LIMIT_UNIT_CODE, design_flow_round) 
