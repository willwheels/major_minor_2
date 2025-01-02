library(tidytable)
library(ggplot2)

## Courtney suggests LIMIT_ID

load(here::here("data", "R_data_files", "all_potw_permits.Rda"))

source(here::here("theme_tina.R"))

icis_permits2 <- icis_permits2 %>%
  select(EXTERNAL_PERMIT_NMBR, VERSION_NMBR, design_flow_round_one_decimal)


read_dmr_data <- function(dmr_year) {
  
  print(dmr_year)
  
  dmr_file <- paste0( "dmr_data_", dmr_year,".Rda")
  
  load(here::here("data", "R_data_files", dmr_file))
  
  dmr_data <- dmr_data %>%
    filter(PERM_FEATURE_TYPE_CODE == "EXO", LIMIT_TYPE_CODE == "ENF",
           MONITORING_LOCATION_CODE %in% c("1", "2", "EG", "Y"),
           !is.na(LIMIT_VALUE_NMBR)) %>%
    filter(!(is.na(DMR_VALUE_NMBR) & is.na(NODI_CODE))) %>%
    select(EXTERNAL_PERMIT_NMBR, VERSION_NMBR, PERM_FEATURE_NMBR, MONITORING_LOCATION_CODE, 
          # LIMIT_ID, 
           LIMIT_VALUE_TYPE_CODE, LIMIT_VALUE_ID, LIMIT_SET_DESIGNATOR, 
           LIMIT_VALUE_NMBR, LIMIT_UNIT_CODE, DMR_VALUE_NMBR, DMR_UNIT_CODE,
           PARAMETER_CODE, PARAMETER_DESC, STATISTICAL_BASE_CODE, LIMIT_VALUE_QUALIFIER_CODE,
           VIOLATION_CODE, NODI_CODE,
           monitoring_period_end_date2) %>%
    distinct() |>
    left_join(icis_permits2) %>%
    filter(!is.na(design_flow_round_one_decimal)) |>
    mutate(data_year = dmr_year)
  
  inv_gc()
  
  return(dmr_data)
    
}


dmr_years <- as.character(2020:2023)

all_dmrs <- purrr::map(dmr_years, read_dmr_data)

all_dmrs <- all_dmrs |>
  bind_rows()

look_at_dists <- all_dmrs |>
  filter(design_flow_round_one_decimal >= .8, design_flow_round_one_decimal <= 1.1,
         PARAMETER_CODE %in% c("00530", "80082", "00610"),
         !is.na(DMR_VALUE_NMBR)) |>
  mutate(effluent_ratio = DMR_VALUE_NMBR/LIMIT_VALUE_NMBR)

ggplot(look_at_dists |> filter(PARAMETER_CODE == "00530", effluent_ratio < 5),
       aes(x = effluent_ratio, group = design_flow_round_one_decimal, color = design_flow_round_one_decimal)) +
  geom_density() + 
  theme_minimal() +
  scale_color_viridis_c()

ggplot(look_at_dists |> filter(PARAMETER_CODE == "80082", effluent_ratio < 10),
       aes(x = effluent_ratio, group = design_flow_round_one_decimal, color = design_flow_round_one_decimal)) +
  geom_density() + 
  theme_minimal() +
  scale_color_viridis_c()

test2 <- all_dmrs|>
  filter(!is.na(DMR_VALUE_NMBR), !is.na(LIMIT_VALUE_NMBR)) |>
  group_by(EXTERNAL_PERMIT_NMBR) |>
  mutate(num_flow_limits = sum(!is.na(LIMIT_VALUE_NMBR) & PARAMETER_CODE == "50050"),
         any_flow_limits = num_flow_limits > 1) |>
  ungroup() |>
  group_by(EXTERNAL_PERMIT_NMBR, VERSION_NMBR, PERM_FEATURE_NMBR, MONITORING_LOCATION_CODE,
           LIMIT_SET_DESIGNATOR,
           PARAMETER_CODE, PARAMETER_DESC, STATISTICAL_BASE_CODE, monitoring_period_end_date2, data_year) |>
  distinct() |>
  mutate(num_lines = n(), group_id = cur_group_id()) |>
  ungroup() |>
  filter(num_lines == 2) |>
  mutate(limit_value_type_code_short = substr(LIMIT_VALUE_TYPE_CODE, 1, 1)) |>
  group_by(EXTERNAL_PERMIT_NMBR, group_id) |>
  filter(any(limit_value_type_code_short == "C") & any(limit_value_type_code_short == "Q"),
         ) |>
  ungroup() |>
  pivot_wider(id_cols = c(group_id, design_flow_round_one_decimal, data_year, 
                          num_flow_limits, any_flow_limits), 
              names_from = limit_value_type_code_short, 
              values_from = c(LIMIT_VALUE_NMBR, LIMIT_UNIT_CODE, DMR_VALUE_NMBR, DMR_UNIT_CODE)) |>
  mutate(effluent_ratio_c = DMR_VALUE_NMBR_C/LIMIT_VALUE_NMBR_C,
         effluent_ratio_q = DMR_VALUE_NMBR_Q/LIMIT_VALUE_NMBR_Q) |>
  filter(effluent_ratio_q < quantile(effluent_ratio_q, .999),
         effluent_ratio_q > quantile(effluent_ratio_q, .001)) |>
  filter(effluent_ratio_c < quantile(effluent_ratio_q, .999),
         effluent_ratio_c > quantile(effluent_ratio_q, .001)) |>
  mutate(effl_ratio_diff_q_minus_c = effluent_ratio_q - effluent_ratio_c)


dmr_data_summ <- test2 |>
  group_by(design_flow_round_one_decimal, data_year) |>
  summarise(mean_effl_ratio_c = mean(effluent_ratio_c, na.rm = TRUE),
            mean_effl_ratio_q = mean(effluent_ratio_q, na.rm = TRUE),
            mean_effl_ratio_q_minus_c = mean(effl_ratio_diff_q_minus_c, na.rm = TRUE),
            sd_effl_ratio_c = sd(effluent_ratio_c, na.rm = TRUE),
            sd_effl_ratio_q = sd(effluent_ratio_q, na.rm = TRUE),
            sd_effl_ratio_q_minus_c = sd(effl_ratio_diff_q_minus_c, na.rm = TRUE),
            num_in_bin = n()
            
            ) |>
  ungroup()

ggplot(dmr_data_summ |> filter(design_flow_round_one_decimal <2.5) , 
       aes(x = design_flow_round_one_decimal, y = mean_effl_ratio_c)) +
  geom_point(aes(size = num_in_bin)) +
  geom_errorbar(aes(ymin = mean_effl_ratio_c - 1.96*sd_effl_ratio_c/sqrt(num_in_bin), 
                    ymax = mean_effl_ratio_c + 1.96*sd_effl_ratio_c/sqrt(num_in_bin))) +
  labs(title = "Mean Concentration Effluent Ratios by Design Flow and Year") +
  theme_tina +
  facet_wrap(~data_year)

ggsave(file = here::here("figs", "conc_ratios_paired_obs.png"),
       h = 8.5, w = 11, units = "in", bg = "white")


ggplot(dmr_data_summ |> filter(design_flow_round_one_decimal <2.5) , 
       aes(x = design_flow_round_one_decimal, y = mean_effl_ratio_q)) +
  geom_point(aes(size = num_in_bin)) +
  geom_errorbar(aes(ymin = mean_effl_ratio_q - 1.96*sd_effl_ratio_q/sqrt(num_in_bin), 
                    ymax = mean_effl_ratio_q + 1.96*sd_effl_ratio_q/sqrt(num_in_bin))) +
  labs(title = "Mean Quantity Effluent Ratios by Design Flow and Year") +
  theme_tina +
  facet_wrap(~data_year)


ggsave(file = here::here("figs", "quant_ratios_paired_obs.png"),
       h = 8.5, w = 11, units = "in", bg = "white")


ggplot(dmr_data_summ |> filter(design_flow_round_one_decimal <2.5) , 
       aes(x = design_flow_round_one_decimal, y = mean_effl_ratio_q_minus_c)) +
  geom_point(aes(size = num_in_bin)) +
  geom_errorbar(aes(ymin = mean_effl_ratio_q_minus_c - 1.96*sd_effl_ratio_q_minus_c/sqrt(num_in_bin), 
                    ymax = mean_effl_ratio_q_minus_c + 1.96*sd_effl_ratio_q_minus_c/sqrt(num_in_bin))) +
  labs(title = "Mean Q-C Effluent Ratio Differences by Design Flow and Year") +
  theme_tina +
  facet_wrap(~data_year)


ggsave(file = here::here("figs", "quant_conc_ratio_diffs_paired_obs.png"),
       h = 8.5, w = 11, units = "in", bg = "white")



dmr_data_summ2 <- test2 |>
  group_by(design_flow_round_one_decimal, any_flow_limits) |>
  summarise(mean_effl_ratio_c = mean(effluent_ratio_c, na.rm = TRUE),
            mean_effl_ratio_q = mean(effluent_ratio_q, na.rm = TRUE),
            mean_effl_ratio_q_minus_c = mean(effl_ratio_diff_q_minus_c, na.rm = TRUE),
            sd_effl_ratio_c = sd(effluent_ratio_c, na.rm = TRUE),
            sd_effl_ratio_q = sd(effluent_ratio_q, na.rm = TRUE),
            sd_effl_ratio_q_minus_c = sd(effl_ratio_diff_q_minus_c, na.rm = TRUE),
            num_in_bin = n()
            
  ) |>
  ungroup()

ggplot(dmr_data_summ2 |> filter(design_flow_round_one_decimal < 2.5),
       aes(x = design_flow_round_one_decimal, y = mean_effl_ratio_c)) +
  geom_point(aes(size = num_in_bin)) +
  geom_errorbar(aes(ymin = mean_effl_ratio_c - 1.96*sd_effl_ratio_c/sqrt(num_in_bin), 
                    ymax = mean_effl_ratio_c + 1.96*sd_effl_ratio_c/sqrt(num_in_bin))) +
  labs(title = "Mean Concentration Effluent Ratios by Design Flow and Presence of Flow Limits") +
  theme_tina +
  facet_wrap(~any_flow_limits)


ggsave(file = here::here("figs", "conc_ratios_paired_obs_by_flow_limits.png"),
       h = 8.5, w = 11, units = "in", bg = "white")


ggplot(dmr_data_summ2 |> filter(design_flow_round_one_decimal < 2.5) , 
       aes(x = design_flow_round_one_decimal, y = mean_effl_ratio_q)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_effl_ratio_q - 1.96*sd_effl_ratio_q/sqrt(num_in_bin), 
                    ymax = mean_effl_ratio_q + 1.96*sd_effl_ratio_q/sqrt(num_in_bin))) +
  
  labs(title = "Mean Quantity Effluent Ratios by Design Flow") +
  theme_tina +
  facet_wrap(~any_flow_limits)


ggsave(file = here::here("figs", "quant_ratios_paired_obs_by_flow_limits.png"),
       h = 8.5, w = 11, units = "in", bg = "white")



ggplot(dmr_data_summ2 |> filter(design_flow_round_one_decimal <3) , 
       aes(x = design_flow_round_one_decimal, y = mean_effl_ratio_q_minus_c
           )) +
  geom_point(aes(size = num_in_bin)) +
  geom_errorbar(aes(ymin = mean_effl_ratio_q_minus_c - 1.96*sd_effl_ratio_q_minus_c/sqrt(num_in_bin), 
                                  ymax = mean_effl_ratio_q_minus_c + 1.96*sd_effl_ratio_q_minus_c/sqrt(num_in_bin))) +
  
  labs(title = "Mean Q-C Effluent Ratio Differences by Design Flow and Presence of Flow Limits") +
  theme_tina +
  facet_wrap(~any_flow_limits)


ggsave(file = here::here("figs", "quant_conc_ratio_diffss_paired_obs_by_flow_limits.png"),
       h = 8.5, w = 11, units = "in", bg = "white")


######


load(here::here("data", "icis_ref_unit_convert.Rda"))
load(here::here("data", "icis_ref_unit.Rda"))

icis_ref_unit <- icis_ref_unit |>
  select(1:3)

icis_convert_quantities <- icis_ref_unit_convert |>
  select(1:3) |>
  filter(TARGET_UNIT_CODE == "26") |>
  rename(converstion_factor_q = CONVERSION_FACTOR)

icis_convert_concentrations <- icis_ref_unit_convert |>
  select(1:3) |>
  filter(TARGET_UNIT_CODE == "19")  |>
  rename(converstion_factor_c = CONVERSION_FACTOR)

back_out_flow <- all_dmrs |>
  filter(!is.na(DMR_VALUE_NMBR), !is.na(LIMIT_VALUE_NMBR)) |>
         #PERM_FEATURE_NMBR == "001") |>    #### THIS IS DIFFERENT
  group_by(EXTERNAL_PERMIT_NMBR) |>
  mutate(num_flow_limits = sum(!is.na(LIMIT_VALUE_NMBR) & PARAMETER_CODE == "50050"),
         any_flow_limits = num_flow_limits > 1) |>
  ungroup() |>
  group_by(EXTERNAL_PERMIT_NMBR, VERSION_NMBR, PERM_FEATURE_NMBR, MONITORING_LOCATION_CODE,
           LIMIT_SET_DESIGNATOR,
           PARAMETER_CODE, PARAMETER_DESC, STATISTICAL_BASE_CODE, monitoring_period_end_date2, data_year) |>
  distinct() |>
  mutate(num_lines = n(), group_id = cur_group_id()) |>
  ungroup() |>
  filter(num_lines == 2) |>
  mutate(limit_value_type_code_short = substr(LIMIT_VALUE_TYPE_CODE, 1, 1)) |>
  group_by(group_id) |>
  filter(any(limit_value_type_code_short == "C") & any(limit_value_type_code_short == "Q"),
  ) |>
  ungroup() |>
  pivot_wider(id_cols = c(EXTERNAL_PERMIT_NMBR, group_id, design_flow_round_one_decimal, data_year, 
                          num_flow_limits, any_flow_limits), 
              names_from = limit_value_type_code_short, 
              values_from = c(LIMIT_VALUE_NMBR, LIMIT_UNIT_CODE, DMR_VALUE_NMBR, DMR_UNIT_CODE)) |>
  mutate(effluent_ratio_c = DMR_VALUE_NMBR_C/LIMIT_VALUE_NMBR_C,
         effluent_ratio_q = DMR_VALUE_NMBR_Q/LIMIT_VALUE_NMBR_Q) |>
  filter(effluent_ratio_q < quantile(effluent_ratio_q, .999),
         effluent_ratio_q > quantile(effluent_ratio_q, .001)) |>
  filter(effluent_ratio_c < quantile(effluent_ratio_q, .999),
         effluent_ratio_c > quantile(effluent_ratio_q, .001)) |>
  mutate(effl_ratio_diff_q_minus_c = effluent_ratio_q - effluent_ratio_c)  |>
  left_join(icis_convert_quantities, by = c("LIMIT_UNIT_CODE_Q" = "SOURCE_UNIT_CODE")) |>
  select(-TARGET_UNIT_CODE) |>
  left_join(icis_convert_concentrations, by = c("LIMIT_UNIT_CODE_C" = "SOURCE_UNIT_CODE")) |>
  mutate(converstion_factor_q = if_else(DMR_UNIT_CODE_Q == "26", 1, converstion_factor_q),
         converstion_factor_c = if_else(DMR_UNIT_CODE_C == "19", 1, converstion_factor_q),
         limit_q_value_convert = LIMIT_VALUE_NMBR_Q*converstion_factor_q,
         dmr_q_value_convert = DMR_VALUE_NMBR_Q*converstion_factor_q,
         limit_c_value_convert = LIMIT_VALUE_NMBR_C*converstion_factor_q,
         dmr_c_value_convert = DMR_VALUE_NMBR_C*converstion_factor_q,
         backed_out_flow_MGD_limit = limit_q_value_convert/(limit_c_value_convert*8.43),
         backed_out_flow_MGD_dmr = dmr_q_value_convert/(dmr_c_value_convert*8.43))

summary(back_out_flow)

back_out_flow_fac_level <- back_out_flow |>
  #group_by(EXTERNAL_PERMIT_NMBR, group_id, design_flow_round_one_decimal, any_flow_limits) |>
  group_by(EXTERNAL_PERMIT_NMBR, design_flow_round_one_decimal, any_flow_limits) |>
  summarise(mean_back_out_flow_limit = mean(backed_out_flow_MGD_limit, na.rm = TRUE),
            mean_back_out_flow_dmr = mean(backed_out_flow_MGD_dmr, na.rm = TRUE)
  ) |>
  ungroup() |>
  mutate(multiple_dmr = mean_back_out_flow_dmr/design_flow_round_one_decimal,
         multiple_limit = mean_back_out_flow_limit/design_flow_round_one_decimal) |>
  filter(!(design_flow_round_one_decimal > 0 & multiple_limit > 100))

summary(back_out_flow_fac_level)


back_out_flow_summ <- back_out_flow_fac_level |>
  group_by(design_flow_round_one_decimal, any_flow_limits)|>
  summarise(mean_back_out_flow_limit = mean(mean_back_out_flow_limit, na.rm = TRUE),
            mean_back_out_flow_dmr = mean(mean_back_out_flow_dmr, na.rm = TRUE),
            sd_back_out_flow_limit = sd(mean_back_out_flow_limit, na.rm = TRUE),
            sd_back_out_flow_dmr = sd(mean_back_out_flow_dmr, na.rm = TRUE),
            num_in_bin = n()
  ) |>
  ungroup()


ggplot(back_out_flow_summ |> filter(design_flow_round_one_decimal <= 2.5), 
       aes(x = design_flow_round_one_decimal, y = mean_back_out_flow_limit,
           group = any_flow_limits, colour = any_flow_limits)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_back_out_flow_limit - 1.96*sd_back_out_flow_limit/sqrt(num_in_bin), 
                    ymax = mean_back_out_flow_limit + 1.96*sd_back_out_flow_limit/sqrt(num_in_bin))) +
  labs("Backed Out Flow from Limits, in MGD") +
  theme_tina

ggsave(here::here("figs", "backed_out_flow_from_limits.png"), 
                  h = 8.5, w = 11, units = "in", bg = "white")

ggplot(back_out_flow_summ |> filter(design_flow_round_one_decimal <= 2.5), 
       aes(x = design_flow_round_one_decimal, y = mean_back_out_flow_dmr,
           group = any_flow_limits, colour = any_flow_limits)) +
  geom_errorbar(aes(ymin = mean_back_out_flow_dmr - 1.96*sd_back_out_flow_dmr/sqrt(num_in_bin), 
                    ymax = mean_back_out_flow_dmr + 1.96*sd_back_out_flow_dmr/sqrt(num_in_bin))) +
  labs("Backed Out Flow from DMRs, in MGD") +
  geom_point() +
  theme_tina

ggsave(here::here("figs", "backed_out_flow_from_dmrs.png"), 
       h = 8.5, w = 11, units = "in", bg = "white")

# 
# ggplot(back_out_flow_summ |> filter(design_flow_round_one_decimal <= 2.5),
#        aes(x = mean_back_out_flow_dmr, y = mean_back_out_flow_limit,
#            group = any_flow_limits, color = any_flow_limits)) +
#   geom_line() +
#   theme_tina
#        


any_flow_limits <- all_dmrs |>
  group_by(EXTERNAL_PERMIT_NMBR, VERSION_NMBR) |>
  summarise(num_flow_limits = sum((!is.na(LIMIT_VALUE_NMBR) & PARAMETER_CODE == "50050"))) |>
  ungroup() |> 
  mutate(any_flow_limits = num_flow_limits > 1)

save(any_flow_limits, file = here::here("data", "any_flow_limits.Rda"))
