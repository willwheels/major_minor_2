library(tidytable)
library(ggplot2)

load(here::here("data", "R_data_files", "flow_with_freq_and_sample_type.Rda"))

sum(is.na(dmr_flow_data$limit_freq_analysis))
sum(is.na(dmr_flow_data$limit_sample_type))

sum(is.na(dmr_flow_data$dmr_freq_analysis))
sum(is.na(dmr_flow_data$dmr_sample_type))
 
check <- dmr_flow_data |> filter(is.na(limit_freq_analysis))

unique(dmr_flow_data$limit_sample_type)

freq_analysis_counts <- dmr_flow_data |> 
  filter(design_flow_round_one_decimal <= 2) |>
  group_by(limit_freq_analysis, design_flow_round_one_decimal) |>
  count() |>
  ungroup() |>
  group_by(design_flow_round_one_decimal) |>
  mutate(pct_use = n/sum(n)) |>
  ungroup() |>
  arrange(design_flow_round_one_decimal, desc(n))

popular_freq_analysis <- freq_analysis_counts |>
  filter(n > 9000) |>
  select(limit_freq_analysis) 


freq_analysis_counts2 <- freq_analysis_counts |>
  mutate(limit_freq_analysis = if_else(limit_freq_analysis %in% unique(popular_freq_analysis$limit_freq_analysis),
                                       limit_freq_analysis,
                                       "Other"),
         limit_freq_analysis = replace_na(limit_freq_analysis, "NA")) |>
  group_by(limit_freq_analysis, design_flow_round_one_decimal) |>
  summarize(n = sum(n)) |>
  ungroup()


ggplot(freq_analysis_counts2, 
       aes(fill=limit_freq_analysis, y=n, x=design_flow_round_one_decimal)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis_d()+
  theme_minimal()

#do in each proporations of continious

sample_type_counts <- dmr_flow_data |> 
  filter(design_flow_round_one_decimal <= 2) |>
  group_by(limit_sample_type, design_flow_round_one_decimal) |>
  count(sort = TRUE) |>
  ungroup()

popular_sample_types <- sample_type_counts |>
  filter(n > 5000) |>
  select(limit_sample_type) 

sample_type_counts2 <- sample_type_counts |>
  mutate(limit_sample_type = if_else(limit_sample_type %in% unique(popular_sample_types$limit_sample_type),
                                       limit_sample_type,
                                       "Other"),
         limit_sample_type = replace_na(limit_sample_type, "NA")) |>
  group_by(limit_sample_type, design_flow_round_one_decimal) |>
  summarize(n = sum(n)) |>
  ungroup()

ggplot(sample_type_counts2, 
       aes(fill=limit_sample_type, y=n, x=design_flow_round_one_decimal)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis_d()+
  theme_minimal()


## to do: consider replacing limit_ variables with dmr if missing; or perhaps dmr is first

LA_and_TX <- dmr_flow_data |>
  filter(substr(EXTERNAL_PERMIT_NMBR, 1, 2) %in% c("LA", "TX"))

sample_type_counts_LA_TX <- LA_and_TX |> 
  filter(design_flow_round_one_decimal <= 2) |>
  group_by(limit_sample_type, design_flow_round_one_decimal) |>
  count(sort = TRUE) |>
  ungroup()

ggplot(sample_type_counts_LA_TX, 
       aes(fill=limit_sample_type, y=n, x=design_flow_round_one_decimal)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis_d()+
  theme_minimal()


## looks like we need to go state by state...