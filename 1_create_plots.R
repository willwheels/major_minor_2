## This script creates figures for flow and conventional pollutant quantity and concentration


library(readr)
library(dplyr)
library(sqldf)
library(magrittr)
library(tidylog) #wrapper for tidy that gives information on each output
library(tidytable)
library(udpipe)
library(ggplot2)

## Get info on permit components to determine POTWs
perm_components <- fread(here::here("data", "csv_files", "NPDES_PERM_COMPONENTS.csv"))
colnames(perm_components) <- tolower(colnames(perm_components))

perm_components <- perm_components %>%
  select(-component_type_code) %>%
  filter(component_type_desc == "POTW")

perm_components_pretreat <- fread(here::here("data", "csv_files", "NPDES_PERM_COMPONENTS.csv")) %>%
  rename_with(tolower) %>%
  select(-component_type_code) %>%
  filter(component_type_desc == "Pretreatment") %>%
  mutate(pretreat_flag = 1) %>%
  select(-component_type_desc) 

# Flow plots ---
load(here::here("data", "R_data_files", "flow_dmrs.Rda"))
load(here::here("data", "R_data_files", "all_potw_permits.Rda"))
colnames(icis_permits2) <- tolower(colnames(icis_permits2))

icis_permits2 <- icis_permits2 %>%
  select(external_permit_nmbr, version_nmbr, component_type_desc,
         design_flow_round_one_decimal, design_flow_round_two_decimals,
         design_flow_round_three_decimals)

flow_icis <- flow_icis %>%
  left_join(icis_permits2, by=c("external_permit_nmbr", "version_nmbr")) %>%
  filter(!(permit_status_code %in% c("NON", "TRM"))) %>%
  filter(facility_type_indicator == "POTW" | component_type_desc == "POTW") %>%
  left_join(perm_components_pretreat, by = "external_permit_nmbr") %>%
  mutate(pretreat_flag = if_else(is.na(pretreat_flag), 0, 1)) %>%
  arrange(external_permit_nmbr, version_nmbr, monitoring_period_end_date2) %>%
  filter(!is.na(total_design_flow_nmbr)) %>%
  mutate(flow_over_design = flow_correct / total_design_flow_nmbr)
         

flow_icis2 <- flow_icis %>%
  filter(flow_over_design <= 100 & flow_correct <= 100) %>%
  #filter(flow_over_design <= quantile(flow_over_design, 0.99, na.rm = TRUE) & flow_correct <= quantile(flow_correct, 0.99, na.rm = TRUE)) %>%
  filter(total_design_flow_nmbr <= 2) %>%
  group_by(design_flow_round_one_decimal) %>%
  summarise(mean_flow = mean(flow_correct, na.rm = TRUE),
            sd_flow = sd(flow_correct, na.rm = TRUE),
            mean_flow_over_design = mean(flow_over_design, na.rm = TRUE),
            sd_flow_over_design = sd(flow_over_design, na.rm = TRUE),
            median_flow = median(flow_correct, na.rm = TRUE),
            median_flow_over_design = median(flow_over_design, na.rm = TRUE),
            num_in_bin = n()) %>%
  arrange(desc(design_flow_round_one_decimal)) %>%
  ungroup()


a <- ggplot(flow_icis2, aes(x = design_flow_round_one_decimal, y = mean_flow, size = num_in_bin)) +
  geom_point(color = "#0082CB", alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_flow - 1.96*sd_flow/sqrt(num_in_bin), 
                ymax = mean_flow + 1.96*sd_flow/sqrt(num_in_bin)),
                width = 0.03, size = 0.4, show.legend = FALSE) +
  annotate("text", x = 1.05, y = max(flow_icis2$mean_flow) + 0.1, label = "1 MGD", size = 10) +
  geom_vline(xintercept = 1, linetype = 2) +
  labs(size = "No. Permits in Bin" ) +
  ylab("Mean Flow (MGD)") + xlab("Design Flow (MGD)") +
  theme_minimal()

a <- a + theme(legend.position="bottom")
a <- a + theme(text=element_text(size=10))
a <- a + theme(axis.text=element_text(size=20))
a <- a + theme(axis.title.x = element_text(size=30, margin=margin(20,0,0,0)))
a <- a + theme(axis.title.y = element_text(size=30, margin=margin(0,20,0,0)))
a <- a + theme(legend.key.size = unit(3, 'mm'), #change legend key size
               legend.title = element_text(size=20), #change legend title font size
               legend.text = element_text(size=20), #change legend text font size
               legend.title.align = 0.5,
               legend.margin=margin(10,0,0,0)) 
a <- a + theme(plot.margin = margin(1,1,1.5,1.2,"cm"))
a
ggsave(here::here("results", "flow_mean_flow_by_design_flow.png"), dpi = 300, plot=a, h = 8.5, w = 11, units = "in", bg = "white")

b <- ggplot(flow_icis2, aes(x = design_flow_round_one_decimal, y = mean_flow_over_design, size = num_in_bin)) +
  geom_point(color = "#0082CB", alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_flow_over_design - 1.96*sd_flow_over_design/sqrt(num_in_bin), 
                    ymax = mean_flow_over_design + 1.96*sd_flow_over_design/sqrt(num_in_bin)),
                width = 0.03, size = 0.4, show.legend = FALSE) +
  annotate("text", x = 1.05, y = max(flow_icis2$mean_flow) + 0.1, label = "1 MGD", size = 10) +
  geom_vline(xintercept = 1, linetype = 2) +
  labs(size = "No. Permits in Bin" ) +
  ylab("Mean Flow/Design Flow (MGD)") + xlab("Design Flow (MGD)") +
  theme_minimal()

b <- b + theme(legend.position="bottom")
b <- b + theme(text=element_text(size=10))
b <- b + theme(axis.text=element_text(size=20))
b <- b + theme(axis.title.x = element_text(size=30, margin=margin(20,0,0,0)))
b <- b + theme(axis.title.y = element_text(size=30, margin=margin(0,20,0,0)))
b <- b + theme(legend.key.size = unit(3, 'mm'), #change legend key size
               legend.title = element_text(size=20), #change legend title font size
               legend.text = element_text(size=20), #change legend text font size
               legend.title.align = 0.5,
               legend.margin=margin(10,0,0,0)) 
b <- b + theme(plot.margin = margin(1,1,1.5,1.2,"cm"))
b
ggsave(here::here("results", "flow_mean_flow_over_design_by_design_flow.png"), dpi = 300, plot=b, h = 8.5, w = 11, units = "in", bg = "white")

c <- ggplot(flow_icis2, aes(x = design_flow_round_one_decimal, y = median_flow, size = num_in_bin)) +
  geom_point(color = "#0082CB") +
  annotate("text", x = 1.15, y = 1, label = "1 MGD", size = 10) +
  geom_vline(xintercept = 1, linetype = 2) +
  labs(size = "No. Permits in Bin" ) +
  ylab("Median Flow (MGD)") + xlab("Design Flow (MGD)") +
  theme_minimal()

c <- c + theme(legend.position="bottom")
c <- c + theme(text=element_text(size=30))
c <- c + theme(axis.text=element_text(size=30))
c <- c + theme(axis.title.x = element_text(size=40, margin=margin(30,0,0,0)))
c <- c + theme(axis.title.y = element_text(size=40, margin=margin(0,30,0,0)))
c <- c + theme(legend.key.size = unit(3, 'mm'), #change legend key size
               legend.title = element_text(size=30), #change legend title font size
               legend.text = element_text(size=30), #change legend text font size
               legend.title.align = 0.5,
               legend.margin=margin(10,0,0,0)) 
c
ggsave(here::here("results", "median_flow_by_design_flow.png"), dpi = 300, plot=c, h = 8.5, w = 11, units = "in", bg = "white")

d <- ggplot(flow_icis2, aes(x = design_flow_round_one_decimal, y = median_flow_over_design, size = num_in_bin)) +
  geom_point(color = "#0082CB") +
  annotate("text", x = 1.15, y = 1, label = "1 MGD", size = 10) +
  geom_vline(xintercept = 1, linetype = 2) +
  labs(size = "No. Permits in Bin" ) +
  ylab("Median Flow/Design Flow (MGD)") + xlab("Design Flow (MGD)") +
  theme_minimal()

d <- d + theme(legend.position="bottom")
d <- d + theme(text=element_text(size=30))
d <- d + theme(axis.text=element_text(size=30))
d <- d + theme(axis.title.x = element_text(size=40, margin=margin(30,0,0,0)))
d <- d + theme(axis.title.y = element_text(size=40, margin=margin(0,30,0,0)))
d <- d + theme(legend.key.size = unit(3, 'mm'), #change legend key size
               legend.title = element_text(size=30), #change legend title font size
               legend.text = element_text(size=30), #change legend text font size
               legend.title.align = 0.5,
               legend.margin=margin(10,0,0,0)) 
d
ggsave(here::here("results", "median_flow_over_design_by_design_flow.png"), dpi = 300, plot=d, h = 8.5, w = 11, units = "in", bg = "white")



# Mean concentration plots ----

load(here::here("data", "R_data_files", "mean_conc.Rda"))
load(here::here("data", "R_data_files", "max_conc.Rda"))
load(here::here("data", "R_data_files", "mean_quant.Rda"))
load(here::here("data", "R_data_files", "max_quant.Rda"))

  

prep_mean_conc <- function(type, pollutant){
  
  assign("data", type)
  
  data <- data %>%
    filter(pollutant_type == pollutant) %>%
    filter(total_design_flow_nmbr <= 2) %>%
    group_by(design_flow_round) %>%
    summarise(sd_mean_conc = sd(mean_conc, na.rm = TRUE),
              mean_conc = mean(mean_conc, na.rm = TRUE),
              num_in_bin = n(),
              pollutant_type = first(pollutant_type)) %>%
    ungroup() %>%
  
  return(data)
    
}

prep_max_conc <- function(type, pollutant){
  
  assign("data", type)
  
  data <- data %>%
    filter(pollutant_type == pollutant) %>%
    filter(total_design_flow_nmbr <= 2) %>%
    group_by(design_flow_round) %>%
    summarise(sd_max_conc = sd(max_conc, na.rm = TRUE),
              mean_max_conc = mean(max_conc, na.rm = TRUE),
              num_in_bin = n(),
              pollutant_type = first(pollutant_type)) %>%
    ungroup() %>%
    
    return(data)
  
}

prep_mean_quant <- function(type, pollutant){
  
  assign("data", type)
  
  data <- data %>%
    filter(pollutant_type == pollutant) %>%
    filter(total_design_flow_nmbr <= 2) %>%
    group_by(design_flow_round) %>%
    summarise(sd_mean_quant = sd(mean_quant, na.rm = TRUE),
              mean_quant = mean(mean_quant, na.rm = TRUE),
              num_in_bin = n(),
              pollutant_type = first(pollutant_type)) %>%
    ungroup() %>%
    
    return(data)
  
}


prep_max_quant <- function(type, pollutant){
  
  assign("data", type)
  
  data <- data %>%
    filter(pollutant_type == pollutant) %>%
    filter(total_design_flow_nmbr <= 2) %>%
    group_by(design_flow_round) %>%
    summarise(sd_max_quant = sd(max_quant, na.rm = TRUE),
              mean_max_quant = mean(max_quant, na.rm = TRUE),
              num_in_bin = n(),
              pollutant_type = first(pollutant_type)) %>%
    ungroup() %>%
    
    return(data)
  
}

dir_create_if_not_exist <- function(dir_name, ...) {
  
  
  if (!dir.exists(here::here(dir_name, ...))) {dir.create(here::here(dir_name, ...))}
}

dir_create_if_not_exist("results")
  


# BOD plots ----
  
BOD_c2 <- prep_mean_conc(type = c2, pollutant = "BOD")
BOD_c3 <- prep_max_conc(type = c3, pollutant = "BOD")
BOD_q1 <- prep_mean_quant(type = q1, pollutant = "BOD")
BOD_q2 <- prep_max_quant(type = q2, pollutant = "BOD")

## Mean BOD Conc. ----
plot <- ggplot(BOD_c2, aes(x = design_flow_round, y = mean_conc, size = num_in_bin)) +
  geom_point(color = "#0082CB", alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_conc - 1.96*sd_mean_conc/sqrt(num_in_bin), 
                    ymax = mean_conc + 1.96*sd_mean_conc/sqrt(num_in_bin)),
                width = 0.03, size = 0.4, show.legend = FALSE) +
  annotate("text", x = 1.05, y = max(BOD_c2$mean_conc)*1.2, label = "1 MGD", size = 10) +
  geom_vline(xintercept = 1, linetype = 2) +
  labs(size = "No. Permits in Bin" ) +
  ylab("Mean BOD Conc. (mg/L)") + xlab("Design Flow (MGD)") +
  theme_minimal()

plot <- plot + theme(legend.position="bottom")
plot <- plot + theme(text=element_text(size=10))
plot <- plot + theme(axis.text=element_text(size=20))
plot <- plot + theme(axis.title.x = element_text(size=30, margin=margin(20,0,0,0)))
plot <- plot + theme(axis.title.y = element_text(size=30, margin=margin(0,20,0,0)))
plot <- plot + theme(legend.key.size = unit(3, 'mm'), #change legend key size
                     legend.title = element_text(size=20), #change legend title font size
                     legend.text = element_text(size=20), #change legend text font size
                     legend.title.align = 0.5,
                     legend.margin=margin(10,0,0,0)) 
plot <- plot + theme(plot.margin = margin(1,1,1.5,1.2,"cm"))
plot
ggsave(here::here("results", "BOD_mean_conc_by_design_flow.png"), dpi = 300, plot=plot, h = 8.5, w = 11, units = "in", bg = "white")


## Mean Max BOD Conc. ----
plot <- ggplot(BOD_c3, aes(x = design_flow_round, y = mean_max_conc, size = num_in_bin)) +
  geom_point(color = "#0082CB", alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_max_conc - 1.96*sd_max_conc/sqrt(num_in_bin), 
                    ymax = mean_max_conc + 1.96*sd_max_conc/sqrt(num_in_bin)),
                width = 0.03, size = 0.4, show.legend = FALSE) +
  annotate("text", x = 1.05, y = max(BOD_c3$mean_max_conc)*1.2, label = "1 MGD", size = 10) +
  geom_vline(xintercept = 1, linetype = 2) +
  labs(size = "No. Permits in Bin" ) +
  ylab("Mean Max BOD Conc. (mg/L)") + xlab("Design Flow (MGD)") +
  theme_minimal()

plot <- plot + theme(legend.position="bottom")
plot <- plot + theme(text=element_text(size=10))
plot <- plot + theme(axis.text=element_text(size=20))
plot <- plot + theme(axis.title.x = element_text(size=25, margin=margin(20,0,0,0)))
plot <- plot + theme(axis.title.y = element_text(size=25, margin=margin(0,20,0,0)))
plot <- plot + theme(legend.key.size = unit(3, 'mm'), #change legend key size
                     legend.title = element_text(size=20), #change legend title font size
                     legend.text = element_text(size=20), #change legend text font size
                     legend.title.align = 0.5,
                     legend.margin=margin(10,0,0,0)) 
plot <- plot + theme(plot.margin = margin(1,1,1.5,1.2,"cm"))
plot
ggsave(here::here("results", "BOD_mean_max_conc_by_design_flow.png"), dpi = 300, plot=plot, h = 8.5, w = 11, units = "in", bg = "white")



## Mean BOD Quant ----
plot <- ggplot(BOD_q1, aes(x = design_flow_round, y = mean_quant, size = num_in_bin)) +
  geom_point(color = "#0082CB", alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_quant - 1.96*sd_mean_quant/sqrt(num_in_bin), 
                    ymax = mean_quant + 1.96*sd_mean_quant/sqrt(num_in_bin)),
                width = 0.03, size = 0.4, show.legend = FALSE) +  
  annotate("text", x = 1.05, y = max(BOD_q1$mean_quant)*1.2, label = "1 MGD", size = 10) +
  geom_vline(xintercept = 1, linetype = 2) +
  labs(size = "No. Permits in Bin" ) +
  ylab("Mean BOD Quant. (kg/d)") + xlab("Design Flow (MGD)") +
  theme_minimal()

plot <- plot + theme(legend.position="bottom")
plot <- plot + theme(text=element_text(size=10))
plot <- plot + theme(axis.text=element_text(size=20))
plot <- plot + theme(axis.title.x = element_text(size=25, margin=margin(20,0,0,0)))
plot <- plot + theme(axis.title.y = element_text(size=25, margin=margin(0,20,0,0)))
plot <- plot + theme(legend.key.size = unit(3, 'mm'), #change legend key size
                     legend.title = element_text(size=20), #change legend title font size
                     legend.text = element_text(size=20), #change legend text font size
                     legend.title.align = 0.5,
                     legend.margin=margin(10,0,0,0)) 
plot <- plot + theme(plot.margin = margin(1,1,1.5,1.2,"cm"))
plot
ggsave(here::here("results", "BOD_mean_quant_by_design_flow.png"), dpi = 300, plot=plot, h = 8.5, w = 11, units = "in", bg = "white")



## Mean Max BOD Quant ----
plot <- ggplot(BOD_q2, aes(x = design_flow_round, y = mean_max_quant, size = num_in_bin)) +
  geom_point(color = "#0082CB", alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_max_quant - 1.96*sd_max_quant/sqrt(num_in_bin), 
                    ymax = mean_max_quant + 1.96*sd_max_quant/sqrt(num_in_bin)),
                width = 0.03, size = 0.4, show.legend = FALSE) +
  annotate("text", x = 1.05, y = max(BOD_q2$mean_max_quant)*1.2, label = "1 MGD", size = 10) +
  geom_vline(xintercept = 1, linetype = 2) +
  labs(size = "No. Permits in Bin" ) +
  ylab("Mean Max BOD Quant. (kg/d)") + xlab("Design Flow (MGD)") +
  theme_minimal()

plot <- plot + theme(legend.position="bottom")
plot <- plot + theme(text=element_text(size=10))
plot <- plot + theme(axis.text=element_text(size=20))
plot <- plot + theme(axis.title.x = element_text(size=25, margin=margin(20,0,0,0)))
plot <- plot + theme(axis.title.y = element_text(size=25, margin=margin(0,20,0,0)))
plot <- plot + theme(legend.key.size = unit(3, 'mm'), #change legend key size
                     legend.title = element_text(size=20), #change legend title font size
                     legend.text = element_text(size=20), #change legend text font size
                     legend.title.align = 0.5,
                     legend.margin=margin(10,0,0,0)) 
plot <- plot + theme(plot.margin = margin(1,1,1.5,1.2,"cm"))
plot
ggsave(here::here("results", "BOD_mean_max_quant_by_design_flow.png"), dpi = 300, plot=plot, h = 8.5, w = 11, units = "in", bg = "white")



# DO plots ----

DO_c2 <- prep_mean_conc(type = c2, pollutant = "DO")
DO_c3 <- prep_max_conc(type = c3, pollutant = "DO")

## Mean DO Conc. ----
plot <- ggplot(DO_c2, aes(x = design_flow_round, y = mean_conc, size = num_in_bin)) +
  geom_point(color = "#0082CB", alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_conc - 1.96*sd_mean_conc/sqrt(num_in_bin), 
                    ymax = mean_conc + 1.96*sd_mean_conc/sqrt(num_in_bin)),
                width = 0.03, size = 0.4, show.legend = FALSE) +
  annotate("text", x = 1.05, y = max(DO_c2$mean_conc)*1.2, label = "1 MGD", size = 10) +
  geom_vline(xintercept = 1, linetype = 2) +
  labs(size = "No. Permits in Bin" ) +
  ylab("Mean DO Conc. (mg/L)") + xlab("Design Flow (MGD)") +
  theme_minimal()

plot <- plot + theme(legend.position="bottom")
plot <- plot + theme(text=element_text(size=10))
plot <- plot + theme(axis.text=element_text(size=20))
plot <- plot + theme(axis.title.x = element_text(size=30, margin=margin(20,0,0,0)))
plot <- plot + theme(axis.title.y = element_text(size=30, margin=margin(0,20,0,0)))
plot <- plot + theme(legend.key.size = unit(3, 'mm'), #change legend key size
                     legend.title = element_text(size=20), #change legend title font size
                     legend.text = element_text(size=20), #change legend text font size
                     legend.title.align = 0.5,
                     legend.margin=margin(10,0,0,0)) 
plot <- plot + theme(plot.margin = margin(1,1,1.5,1.2,"cm"))
plot
ggsave(here::here("results", "DO_mean_conc_by_design_flow.png"), dpi = 300, plot=plot, h = 8.5, w = 11, units = "in", bg = "white")

## Mean Max DO Conc. ----
plot <- ggplot(DO_c3, aes(x = design_flow_round, y = mean_max_conc, size = num_in_bin)) +
  geom_point(color = "#0082CB", alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_max_conc - 1.96*sd_max_conc/sqrt(num_in_bin), 
                    ymax = mean_max_conc + 1.96*sd_max_conc/sqrt(num_in_bin)),
                width = 0.03, size = 0.4, show.legend = FALSE) +
  annotate("text", x = 1.05, y = max(DO_c3$mean_max_conc)*1.2, label = "1 MGD", size = 10) +
  geom_vline(xintercept = 1, linetype = 2) +
  labs(size = "No. Permits in Bin" ) +
  ylab("Mean Max DO Conc. (mg/L)") + xlab("Design Flow (MGD)") +
  theme_minimal()

plot <- plot + theme(legend.position="bottom")
plot <- plot + theme(text=element_text(size=10))
plot <- plot + theme(axis.text=element_text(size=20))
plot <- plot + theme(axis.title.x = element_text(size=25, margin=margin(20,0,0,0)))
plot <- plot + theme(axis.title.y = element_text(size=25, margin=margin(0,20,0,0)))
plot <- plot + theme(legend.key.size = unit(3, 'mm'), #change legend key size
                     legend.title = element_text(size=20), #change legend title font size
                     legend.text = element_text(size=20), #change legend text font size
                     legend.title.align = 0.5,
                     legend.margin=margin(10,0,0,0)) 
plot <- plot + theme(plot.margin = margin(1,1,1.5,1.2,"cm"))
plot
ggsave(here::here("results", "DO_mean_max_conc_by_design_flow.png"), dpi = 300, plot=plot, h = 8.5, w = 11, units = "in", bg = "white")




# TSS plots ----

TSS_c2 <- prep_mean_conc(type = c2, pollutant = "TSS")
TSS_c3 <- prep_max_conc(type = c3, pollutant = "TSS")
TSS_q1 <- prep_mean_quant(type = q1, pollutant = "TSS")
TSS_q2 <- prep_max_quant(type = q2, pollutant = "TSS")

## Mean TSS Conc. ----
plot <- ggplot(TSS_c2, aes(x = design_flow_round, y = mean_conc, size = num_in_bin)) +
  geom_point(color = "#0082CB", alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_conc - 1.96*sd_mean_conc/sqrt(num_in_bin), 
                    ymax = mean_conc + 1.96*sd_mean_conc/sqrt(num_in_bin)),
                width = 0.03, size = 0.4, show.legend = FALSE) +
  annotate("text", x = 1.05, y = max(TSS_c2$mean_conc)*1.2, label = "1 MGD", size = 10) +
  geom_vline(xintercept = 1, linetype = 2) +
  labs(size = "No. Permits in Bin" ) +
  ylab("Mean TSS Conc. (mg/L)") + xlab("Design Flow (MGD)") +
  theme_minimal()

plot <- plot + theme(legend.position="bottom")
plot <- plot + theme(text=element_text(size=10))
plot <- plot + theme(axis.text=element_text(size=20))
plot <- plot + theme(axis.title.x = element_text(size=30, margin=margin(20,0,0,0)))
plot <- plot + theme(axis.title.y = element_text(size=30, margin=margin(0,20,0,0)))
plot <- plot + theme(legend.key.size = unit(3, 'mm'), #change legend key size
                     legend.title = element_text(size=20), #change legend title font size
                     legend.text = element_text(size=20), #change legend text font size
                     legend.title.align = 0.5,
                     legend.margin=margin(10,0,0,0)) 
plot <- plot + theme(plot.margin = margin(1,1,1.5,1.2,"cm"))
plot
ggsave(here::here("results", "TSS_mean_conc_by_design_flow.png"), dpi = 300, plot=plot, h = 8.5, w = 11, units = "in", bg = "white")


## Mean Max TSS Conc. ----
plot <- ggplot(TSS_c3, aes(x = design_flow_round, y = mean_max_conc, size = num_in_bin)) +
  geom_point(color = "#0082CB", alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_max_conc - 1.96*sd_max_conc/sqrt(num_in_bin), 
                    ymax = mean_max_conc + 1.96*sd_max_conc/sqrt(num_in_bin)),
                width = 0.03, size = 0.4, show.legend = FALSE) +
  annotate("text", x = 1.05, y = max(TSS_c3$mean_max_conc)*1.2, label = "1 MGD", size = 10) +
  geom_vline(xintercept = 1, linetype = 2) +
  labs(size = "No. Permits in Bin" ) +
  ylab("Mean Max TSS Conc. (mg/L)") + xlab("Design Flow (MGD)") +
  theme_minimal()

plot <- plot + theme(legend.position="bottom")
plot <- plot + theme(text=element_text(size=10))
plot <- plot + theme(axis.text=element_text(size=20))
plot <- plot + theme(axis.title.x = element_text(size=25, margin=margin(20,0,0,0)))
plot <- plot + theme(axis.title.y = element_text(size=25, margin=margin(0,20,0,0)))
plot <- plot + theme(legend.key.size = unit(3, 'mm'), #change legend key size
                     legend.title = element_text(size=20), #change legend title font size
                     legend.text = element_text(size=20), #change legend text font size
                     legend.title.align = 0.5,
                     legend.margin=margin(10,0,0,0)) 
plot <- plot + theme(plot.margin = margin(1,1,1.5,1.2,"cm"))
plot
ggsave(here::here("results", "TSS_mean_max_conc_by_design_flow.png"), dpi = 300, plot=plot, h = 8.5, w = 11, units = "in", bg = "white")



## Mean TSS Quant ----
plot <- ggplot(TSS_q1, aes(x = design_flow_round, y = mean_quant, size = num_in_bin)) +
  geom_point(color = "#0082CB", alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_quant - 1.96*sd_mean_quant/sqrt(num_in_bin), 
                    ymax = mean_quant + 1.96*sd_mean_quant/sqrt(num_in_bin)),
                width = 0.03, size = 0.4, show.legend = FALSE) +  
  annotate("text", x = 1.05, y = max(TSS_q1$mean_quant)*1.2, label = "1 MGD", size = 10) +
  geom_vline(xintercept = 1, linetype = 2) +
  labs(size = "No. Permits in Bin" ) +
  ylab("Mean TSS Quant. (kg/d)") + xlab("Design Flow (MGD)") +
  theme_minimal()

plot <- plot + theme(legend.position="bottom")
plot <- plot + theme(text=element_text(size=10))
plot <- plot + theme(axis.text=element_text(size=20))
plot <- plot + theme(axis.title.x = element_text(size=25, margin=margin(20,0,0,0)))
plot <- plot + theme(axis.title.y = element_text(size=25, margin=margin(0,20,0,0)))
plot <- plot + theme(legend.key.size = unit(3, 'mm'), #change legend key size
                     legend.title = element_text(size=20), #change legend title font size
                     legend.text = element_text(size=20), #change legend text font size
                     legend.title.align = 0.5,
                     legend.margin=margin(10,0,0,0)) 
plot <- plot + theme(plot.margin = margin(1,1,1.5,1.2,"cm"))
plot
ggsave(here::here("results", "TSS_mean_quant_by_design_flow.png"), dpi = 300, plot=plot, h = 8.5, w = 11, units = "in", bg = "white")



## Mean Max TSS Quant ----
plot <- ggplot(TSS_q2, aes(x = design_flow_round, y = mean_max_quant, size = num_in_bin)) +
  geom_point(color = "#0082CB", alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_max_quant - 1.96*sd_max_quant/sqrt(num_in_bin), 
                    ymax = mean_max_quant + 1.96*sd_max_quant/sqrt(num_in_bin)),
                width = 0.03, size = 0.4, show.legend = FALSE) +
  annotate("text", x = 1.05, y = max(TSS_q2$mean_max_quant)*1.2, label = "1 MGD", size = 10) +
  geom_vline(xintercept = 1, linetype = 2) +
  labs(size = "No. Permits in Bin" ) +
  ylab("Mean Max TSS Quant. (kg/d)") + xlab("Design Flow (MGD)") +
  theme_minimal()

plot <- plot + theme(legend.position="bottom")
plot <- plot + theme(text=element_text(size=10))
plot <- plot + theme(axis.text=element_text(size=20))
plot <- plot + theme(axis.title.x = element_text(size=25, margin=margin(20,0,0,0)))
plot <- plot + theme(axis.title.y = element_text(size=25, margin=margin(0,20,0,0)))
plot <- plot + theme(legend.key.size = unit(3, 'mm'), #change legend key size
                     legend.title = element_text(size=20), #change legend title font size
                     legend.text = element_text(size=20), #change legend text font size
                     legend.title.align = 0.5,
                     legend.margin=margin(10,0,0,0)) 
plot <- plot + theme(plot.margin = margin(1,1,1.5,1.2,"cm"))
plot
ggsave(here::here("results", "TSS_mean_max_quant_by_design_flow.png"), dpi = 300, plot=plot, h = 8.5, w = 11, units = "in", bg = "white")





# TN plots ----

TN_c2 <- prep_mean_conc(type = c2, pollutant = "TN")
TN_c3 <- prep_max_conc(type = c3, pollutant = "TN")
TN_q1 <- prep_mean_quant(type = q1, pollutant = "TN")
TN_q2 <- prep_max_quant(type = q2, pollutant = "TN")

## Mean TN Conc. ----
plot <- ggplot(TN_c2, aes(x = design_flow_round, y = mean_conc, size = num_in_bin)) +
  geom_point(color = "#0082CB", alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_conc - 1.96*sd_mean_conc/sqrt(num_in_bin), 
                    ymax = mean_conc + 1.96*sd_mean_conc/sqrt(num_in_bin)),
                width = 0.03, size = 0.4, show.legend = FALSE) +
  annotate("text", x = 1.05, y = max(TN_c2$mean_conc)*1.2, label = "1 MGD", size = 10) +
  geom_vline(xintercept = 1, linetype = 2) +
  labs(size = "No. Permits in Bin" ) +
  ylab("Mean TN Conc. (mg/L)") + xlab("Design Flow (MGD)") +
  theme_minimal()

plot <- plot + theme(legend.position="bottom")
plot <- plot + theme(text=element_text(size=10))
plot <- plot + theme(axis.text=element_text(size=20))
plot <- plot + theme(axis.title.x = element_text(size=30, margin=margin(20,0,0,0)))
plot <- plot + theme(axis.title.y = element_text(size=30, margin=margin(0,20,0,0)))
plot <- plot + theme(legend.key.size = unit(3, 'mm'), #change legend key size
                     legend.title = element_text(size=20), #change legend title font size
                     legend.text = element_text(size=20), #change legend text font size
                     legend.title.align = 0.5,
                     legend.margin=margin(10,0,0,0)) 
plot <- plot + theme(plot.margin = margin(1,1,1.5,1.2,"cm"))
plot
ggsave(here::here("results", "TN_mean_conc_by_design_flow.png"), dpi = 300, plot=plot, h = 8.5, w = 11, units = "in", bg = "white")


## Mean Max TN Conc. ----
plot <- ggplot(TN_c3, aes(x = design_flow_round, y = mean_max_conc, size = num_in_bin)) +
  geom_point(color = "#0082CB", alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_max_conc - 1.96*sd_max_conc/sqrt(num_in_bin), 
                    ymax = mean_max_conc + 1.96*sd_max_conc/sqrt(num_in_bin)),
                width = 0.03, size = 0.4, show.legend = FALSE) +
  annotate("text", x = 1.05, y = max(TN_c3$mean_max_conc)*1.2, label = "1 MGD", size = 10) +
  geom_vline(xintercept = 1, linetype = 2) +
  labs(size = "No. Permits in Bin" ) +
  ylab("Mean Max TN Conc. (mg/L)") + xlab("Design Flow (MGD)") +
  theme_minimal()

plot <- plot + theme(legend.position="bottom")
plot <- plot + theme(text=element_text(size=10))
plot <- plot + theme(axis.text=element_text(size=20))
plot <- plot + theme(axis.title.x = element_text(size=25, margin=margin(20,0,0,0)))
plot <- plot + theme(axis.title.y = element_text(size=25, margin=margin(0,20,0,0)))
plot <- plot + theme(legend.key.size = unit(3, 'mm'), #change legend key size
                     legend.title = element_text(size=20), #change legend title font size
                     legend.text = element_text(size=20), #change legend text font size
                     legend.title.align = 0.5,
                     legend.margin=margin(10,0,0,0)) 
plot <- plot + theme(plot.margin = margin(1,1,1.5,1.2,"cm"))
plot
ggsave(here::here("results", "TN_mean_max_conc_by_design_flow.png"), dpi = 300, plot=plot, h = 8.5, w = 11, units = "in", bg = "white")



## Mean TN Quant ----
plot <- ggplot(TN_q1, aes(x = design_flow_round, y = mean_quant, size = num_in_bin)) +
  geom_point(color = "#0082CB", alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_quant - 1.96*sd_mean_quant/sqrt(num_in_bin), 
                    ymax = mean_quant + 1.96*sd_mean_quant/sqrt(num_in_bin)),
                width = 0.03, size = 0.4, show.legend = FALSE) +  
  annotate("text", x = 1.05, y = max(TN_q1$mean_quant)*1.2, label = "1 MGD", size = 10) +
  geom_vline(xintercept = 1, linetype = 2) +
  labs(size = "No. Permits in Bin" ) +
  ylab("Mean TN Quant. (kg/d)") + xlab("Design Flow (MGD)") +
  theme_minimal()

plot <- plot + theme(legend.position="bottom")
plot <- plot + theme(text=element_text(size=10))
plot <- plot + theme(axis.text=element_text(size=20))
plot <- plot + theme(axis.title.x = element_text(size=25, margin=margin(20,0,0,0)))
plot <- plot + theme(axis.title.y = element_text(size=25, margin=margin(0,20,0,0)))
plot <- plot + theme(legend.key.size = unit(3, 'mm'), #change legend key size
                     legend.title = element_text(size=20), #change legend title font size
                     legend.text = element_text(size=20), #change legend text font size
                     legend.title.align = 0.5,
                     legend.margin=margin(10,0,0,0)) 
plot <- plot + theme(plot.margin = margin(1,1,1.5,1.2,"cm"))
plot
ggsave(here::here("results", "TN_mean_quant_by_design_flow.png"), dpi = 300, plot=plot, h = 8.5, w = 11, units = "in", bg = "white")



## Mean Max TN Quant ----
plot <- ggplot(TN_q2, aes(x = design_flow_round, y = mean_max_quant, size = num_in_bin)) +
  geom_point(color = "#0082CB", alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_max_quant - 1.96*sd_max_quant/sqrt(num_in_bin), 
                    ymax = mean_max_quant + 1.96*sd_max_quant/sqrt(num_in_bin)),
                width = 0.03, size = 0.4, show.legend = FALSE) +
  annotate("text", x = 1.05, y = max(TN_q2$mean_max_quant)*1.2, label = "1 MGD", size = 10) +
  geom_vline(xintercept = 1, linetype = 2) +
  labs(size = "No. Permits in Bin" ) +
  ylab("Mean Max TN Quant. (kg/d)") + xlab("Design Flow (MGD)") +
  theme_minimal()

plot <- plot + theme(legend.position="bottom")
plot <- plot + theme(text=element_text(size=10))
plot <- plot + theme(axis.text=element_text(size=20))
plot <- plot + theme(axis.title.x = element_text(size=25, margin=margin(20,0,0,0)))
plot <- plot + theme(axis.title.y = element_text(size=25, margin=margin(0,20,0,0)))
plot <- plot + theme(legend.key.size = unit(3, 'mm'), #change legend key size
                     legend.title = element_text(size=20), #change legend title font size
                     legend.text = element_text(size=20), #change legend text font size
                     legend.title.align = 0.5,
                     legend.margin=margin(10,0,0,0)) 
plot <- plot + theme(plot.margin = margin(1,1,1.5,1.2,"cm"))
plot
ggsave(here::here("results", "TN_mean_max_quant_by_design_flow.png"), dpi = 300, plot=plot, h = 8.5, w = 11, units = "in", bg = "white")


# TP plots ----

TP_c2 <- prep_mean_conc(type = c2, pollutant = "TP")
TP_c3 <- prep_max_conc(type = c3, pollutant = "TP")
TP_q1 <- prep_mean_quant(type = q1, pollutant = "TP")
TP_q2 <- prep_max_quant(type = q2, pollutant = "TP")

## Mean TP Conc. ----
plot <- ggplot(TP_c2, aes(x = design_flow_round, y = mean_conc, size = num_in_bin)) +
  geom_point(color = "#0082CB", alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_conc - 1.96*sd_mean_conc/sqrt(num_in_bin), 
                    ymax = mean_conc + 1.96*sd_mean_conc/sqrt(num_in_bin)),
                width = 0.03, size = 0.4, show.legend = FALSE) +
  annotate("text", x = 1.05, y = max(TP_c2$mean_conc)*1.2, label = "1 MGD", size = 10) +
  geom_vline(xintercept = 1, linetype = 2) +
  labs(size = "No. Permits in Bin" ) +
  ylab("Mean TP Conc. (mg/L)") + xlab("Design Flow (MGD)") +
  theme_minimal()

plot <- plot + theme(legend.position="bottom")
plot <- plot + theme(text=element_text(size=10))
plot <- plot + theme(axis.text=element_text(size=20))
plot <- plot + theme(axis.title.x = element_text(size=30, margin=margin(20,0,0,0)))
plot <- plot + theme(axis.title.y = element_text(size=30, margin=margin(0,20,0,0)))
plot <- plot + theme(legend.key.size = unit(3, 'mm'), #change legend key size
                     legend.title = element_text(size=20), #change legend title font size
                     legend.text = element_text(size=20), #change legend text font size
                     legend.title.align = 0.5,
                     legend.margin=margin(10,0,0,0)) 
plot <- plot + theme(plot.margin = margin(1,1,1.5,1.2,"cm"))
plot
ggsave(here::here("results", "TP_mean_conc_by_design_flow.png"), dpi = 300, plot=plot, h = 8.5, w = 11, units = "in", bg = "white")


## Mean Max TP Conc. ----
plot <- ggplot(TP_c3, aes(x = design_flow_round, y = mean_max_conc, size = num_in_bin)) +
  geom_point(color = "#0082CB", alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_max_conc - 1.96*sd_max_conc/sqrt(num_in_bin), 
                    ymax = mean_max_conc + 1.96*sd_max_conc/sqrt(num_in_bin)),
                width = 0.03, size = 0.4, show.legend = FALSE) +
  annotate("text", x = 1.05, y = max(TP_c3$mean_max_conc)*1.2, label = "1 MGD", size = 10) +
  geom_vline(xintercept = 1, linetype = 2) +
  labs(size = "No. Permits in Bin" ) +
  ylab("Mean Max TP Conc. (mg/L)") + xlab("Design Flow (MGD)") +
  theme_minimal()

plot <- plot + theme(legend.position="bottom")
plot <- plot + theme(text=element_text(size=10))
plot <- plot + theme(axis.text=element_text(size=20))
plot <- plot + theme(axis.title.x = element_text(size=25, margin=margin(20,0,0,0)))
plot <- plot + theme(axis.title.y = element_text(size=25, margin=margin(0,20,0,0)))
plot <- plot + theme(legend.key.size = unit(3, 'mm'), #change legend key size
                     legend.title = element_text(size=20), #change legend title font size
                     legend.text = element_text(size=20), #change legend text font size
                     legend.title.align = 0.5,
                     legend.margin=margin(10,0,0,0)) 
plot <- plot + theme(plot.margin = margin(1,1,1.5,1.2,"cm"))
plot
ggsave(here::here("results", "TP_mean_max_conc_by_design_flow.png"), dpi = 300, plot=plot, h = 8.5, w = 11, units = "in", bg = "white")



## Mean TP Quant ----
plot <- ggplot(TP_q1, aes(x = design_flow_round, y = mean_quant, size = num_in_bin)) +
  geom_point(color = "#0082CB", alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_quant - 1.96*sd_mean_quant/sqrt(num_in_bin), 
                    ymax = mean_quant + 1.96*sd_mean_quant/sqrt(num_in_bin)),
                width = 0.03, size = 0.4, show.legend = FALSE) +  
  annotate("text", x = 1.05, y = max(TP_q1$mean_quant)*1.2, label = "1 MGD", size = 10) +
  geom_vline(xintercept = 1, linetype = 2) +
  labs(size = "No. Permits in Bin" ) +
  ylab("Mean TP Quant. (kg/d)") + xlab("Design Flow (MGD)") +
  theme_minimal()

plot <- plot + theme(legend.position="bottom")
plot <- plot + theme(text=element_text(size=10))
plot <- plot + theme(axis.text=element_text(size=20))
plot <- plot + theme(axis.title.x = element_text(size=25, margin=margin(20,0,0,0)))
plot <- plot + theme(axis.title.y = element_text(size=25, margin=margin(0,20,0,0)))
plot <- plot + theme(legend.key.size = unit(3, 'mm'), #change legend key size
                     legend.title = element_text(size=20), #change legend title font size
                     legend.text = element_text(size=20), #change legend text font size
                     legend.title.align = 0.5,
                     legend.margin=margin(10,0,0,0)) 
plot <- plot + theme(plot.margin = margin(1,1,1.5,1.2,"cm"))
plot
ggsave(here::here("results", "TP_mean_quant_by_design_flow.png"), dpi = 300, plot=plot, h = 8.5, w = 11, units = "in", bg = "white")



## Mean Max TP Quant ----
plot <- ggplot(TP_q2, aes(x = design_flow_round, y = mean_max_quant, size = num_in_bin)) +
  geom_point(color = "#0082CB", alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_max_quant - 1.96*sd_max_quant/sqrt(num_in_bin), 
                    ymax = mean_max_quant + 1.96*sd_max_quant/sqrt(num_in_bin)),
                width = 0.03, size = 0.4, show.legend = FALSE) +
  annotate("text", x = 1.05, y = max(TP_q2$mean_max_quant)*1.2, label = "1 MGD", size = 10) +
  geom_vline(xintercept = 1, linetype = 2) +
  labs(size = "No. Permits in Bin" ) +
  ylab("Mean Max TP Quant. (kg/d)") + xlab("Design Flow (MGD)") +
  theme_minimal()

plot <- plot + theme(legend.position="bottom")
plot <- plot + theme(text=element_text(size=10))
plot <- plot + theme(axis.text=element_text(size=20))
plot <- plot + theme(axis.title.x = element_text(size=25, margin=margin(20,0,0,0)))
plot <- plot + theme(axis.title.y = element_text(size=25, margin=margin(0,20,0,0)))
plot <- plot + theme(legend.key.size = unit(3, 'mm'), #change legend key size
                     legend.title = element_text(size=20), #change legend title font size
                     legend.text = element_text(size=20), #change legend text font size
                     legend.title.align = 0.5,
                     legend.margin=margin(10,0,0,0)) 
plot <- plot + theme(plot.margin = margin(1,1,1.5,1.2,"cm"))
plot
ggsave(here::here("results", "TP_mean_max_quant_by_design_flow.png"), dpi = 300, plot=plot, h = 8.5, w = 11, units = "in", bg = "white")
