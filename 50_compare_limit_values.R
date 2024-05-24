library(tidytable)
library(ggplot2)


load(here::here("data", "R_data_files", "dmr_data_2018.Rda"))
load(here::here("data", "R_data_files", "all_potw_permits.Rda"))

icis_permits2 <- icis_permits2 %>%
  select(EXTERNAL_PERMIT_NMBR, VERSION_NMBR, design_flow_round)


dmr_data <- dmr_data %>%
  filter(PERM_FEATURE_TYPE_CODE == "EXO", LIMIT_TYPE_CODE == "ENF",
         #MONITORING_LOCATION_CODE %in% c("1", "2", "EG", "Y"),
         MONITORING_LOCATION_CODE == "1", PERM_FEATURE_NMBR == "001",
         !is.na(LIMIT_VALUE_NMBR)) %>%
  select(EXTERNAL_PERMIT_NMBR, VERSION_NMBR, PERM_FEATURE_NMBR, MONITORING_LOCATION_CODE, LIMIT_VALUE_TYPE_CODE,
          LIMIT_VALUE_STANDARD_UNITS, STANDARD_UNIT_CODE,
         PARAMETER_CODE, PARAMETER_DESC, STATISTICAL_BASE_CODE, LIMIT_VALUE_QUALIFIER_CODE,
         monitoring_period_end_date2) %>%
  distinct() %>%
  left_join(icis_permits2) %>%
  filter(!is.na(design_flow_round))

smol <- head(dmr_data, 100000)

inv_gc()

## consider doing something for months, e.g. seasonal, that don't have limits
dmr_limits <- dmr_data %>%
  filter(PARAMETER_CODE %in% c("00530", "80082", "00610"), STATISTICAL_BASE_CODE == "MK") |>
  mutate(LIMIT_VALUE_STANDARD_UNITS = as.numeric(LIMIT_VALUE_STANDARD_UNITS)) |>
  filter(LIMIT_VALUE_STANDARD_UNITS < 1000000) |>
  distinct() %>%
  group_by(EXTERNAL_PERMIT_NMBR, PARAMETER_CODE, LIMIT_VALUE_STANDARD_UNITS,
            STANDARD_UNIT_CODE, monitoring_period_end_date2) %>% 
  distinct() |>  ## some dupes limit_type_code
  group_by(design_flow_round, PARAMETER_CODE, STANDARD_UNIT_CODE) %>%
  #group_by(PARAMETER_CODE, STATISTICAL_BASE_CODE, LIMIT_UNIT_CODE) %>%
  summarise(mean_limit = mean(LIMIT_VALUE_STANDARD_UNITS),
            median_limit = median(LIMIT_VALUE_STANDARD_UNITS),
            num_in_bin = n()) %>%
  filter(design_flow_round <= 2.0) %>%
  arrange(PARAMETER_CODE, STANDARD_UNIT_CODE, design_flow_round) 


## units; 19 is mg/l, 26 is lbs/day

## TRIM OUTLIERS!!!

ggplot(dmr_limits %>% filter(design_flow_round <= 2, STANDARD_UNIT_CODE == "19"), 
       aes(x = design_flow_round, y = mean_limit, size = num_in_bin)) +
  geom_point(aes(colour = PARAMETER_CODE)) +
  labs(title = "Mean Limit by Design Flow, mg/l monthly averages") +
  ylab("Mean Limit") + xlab("Design Flow (rounded to single decimal)") +
  theme_minimal()

ggsave(here::here("figs", "mean_mgl_limits.png"), h = 8.5, w = 11, units = "in", bg = "white")

ggplot(dmr_limits %>% filter(design_flow_round <= 2, STANDARD_UNIT_CODE == "19"), 
       aes(x = design_flow_round, y = median_limit, size = num_in_bin)) +
  geom_point(aes(colour = PARAMETER_CODE)) +
  labs(title = "Median Limit by Design Flow, mg/l monthly averages") +
  ylab("Mean Limit") + xlab("Design Flow (rounded to single decimal)") +
  theme_minimal()

ggsave(here::here("figs", "median_mgl_limits.png"), h = 8.5, w = 11, units = "in", bg = "white")


ggplot(dmr_limits %>% filter(design_flow_round <= 2, STANDARD_UNIT_CODE == "01"), 
       aes(x = design_flow_round, y = mean_limit, size = num_in_bin, group = PARAMETER_CODE)) +
  geom_point(aes(colour = PARAMETER_CODE)) +
  labs(title = "Mean Limit by Design Flow, kg/day monthly averages") +
  ylab("Mean Limit") + xlab("Design Flow (rounded to single decimal)") +
  theme_minimal()

ggsave(here::here("figs", "mean_kgd_limits.png"), h = 8.5, w = 11, units = "in", bg = "white")

