# Updating GCAM LDV costs with Autonomie data

rm(list=ls()) # clear environment
cat("\014") # clear console
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
working_dir <- getwd()
options(java.parameters = "-Xmx8000m")

library(readr)
library(stringr)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(magrittr)
library(data.table)
library(purrr)
library(devtools)
library(rgcam)
library(readxl)
library(xlsx)
library(gcamdata)

# Read in mapping file ----
# tempo_freight_tech_mapping <- read_csv("./mappings/tempo_freight_tech_mapping.csv")
tempo_passenger_tech_mapping <- read_csv("./mappings/tempo_passenger_tech_mapping.csv")

df <- as_tibble(read.csv(
  "mappings/vehicle_detail_map.csv",
  skip = 0, blank.lines.skip = TRUE, check.names = F))
names(df)<-tolower(names(df))
tempo_autonomie_mapping<-df %>% 
  rename(autonomie.tech = vehicle_detail) %>% 
  select(vehicle_class, autonomie.tech, tempo.tech) %>% 
  filter(tempo.tech != 'not_selected') %>% 
  # update naming to match with autonomie's naming
  mutate(autonomie.tech = if_else(autonomie.tech == 'Conv CI',"Conventional CI", autonomie.tech))

tempo_autonm_gcam_tech_mapping<-tempo_autonomie_mapping %>% 
  filter(vehicle_class == 'LDV') %>% 
  left_join(tempo_passenger_tech_mapping %>% 
              # five classes in autonomie: 'Compact', 'Midsize', 'Midsize SUV', 'Pickup', 'Small SUV'
              # 'Midsize SUV' and 'Small SUV' are mapped to 'SUV' in tempo using averages
              filter(Tempo_Class %in% c("Compact", "Midsize", "Pickup", "SUV")) %>% 
              select(Tempo_Tech, Technology) %>% 
              distinct() %>% 
              arrange(Tempo_Tech), by = c("tempo.tech" = "Tempo_Tech")) %>% 
  mutate(sector = 'Passenger') %>% 
  # Note that input autonomie csv only has LDV, below are lines that deal with MHDV when applicable
  # bind_rows(tempo_autonomie_mapping %>% 
  #             filter(vehicle_class == 'MHDV') %>%
  #             left_join(tempo_freight_tech_mapping %>% 
  #                         mutate(Tempo_Tech = if_else(grepl('BEV',Tempo_Tech), 'BEV', Tempo_Tech)) %>% 
  #                         filter(Tempo_Tech %in% (tempo_autonomie_mapping %>% filter(vehicle_class=='MHDV'))$tempo.tech) %>% 
  #                         select(Tempo_Tech, Technology) %>% 
  #                         distinct(), by = c("tempo.tech" = "Tempo_Tech")) %>% 
  #             mutate(sector = 'Freight')) %>% 
  rename(gcam.tech = Technology) %>% 
  # filter out PHEV, which is not represented in GCAM
  filter(!grepl('EREVPHEV', autonomie.tech)) %>% 
  # adjust tech mapping of 'SplitHEV' to 'Hybrid Liquids'
  mutate(gcam.tech = if_else(autonomie.tech == 'SplitHEV', 'Hybrid Liquids', gcam.tech))

# Read in selected columns from Autonomie spreadsheet, for the "Low" case, 
# only include powertrains that are included in GCAM and convert to 2005$, and rename categories
Auto_LDV <- fread(
  "./inputs/LDV_Autonomie.csv",
  select =  c(
    "Model Years: {years}",
    "Vehicle Class: {string}",
    "Vehicle Powertrain: {string}",
    "Technology Progress: {string}",
    "Vehicle Performance Category: {string}",
    "Vehicle Curb Weight: {Kg}",
    "Vehicle MSRP {2023$}",
    #"Levelized Cost of Driving - LCOD: {$/mile}",
    "Adjusted Fuel Economy, Combined 43/57 - real world, CS, Fuel (Gas.Equivalent): {mile/gallon}",
    "Adjusted Electricity Consumption, Combined 43/57 - real world, CD (Wh/mi)"
  )
) %>%
  # first rename columns
  rename_with(
    ~ c(
      "year",
      "Class",
      "Powertrain",
      "Progress",
      "Performance",
      "Curb_weight_kg",
      "MSRP",
      #"LCOD_$/mile",
      "Real-world_FE_mpgge",
      "Real-world_elec_cons_Wh/mi"
    ),
    c(
      "Model Years: {years}",
      "Vehicle Class: {string}",
      "Vehicle Powertrain: {string}",
      "Technology Progress: {string}",
      "Vehicle Performance Category: {string}",
      "Vehicle Curb Weight: {Kg}",
      "Vehicle MSRP {2023$}",
      #"Levelized Cost of Driving - LCOD: {$/mile}",
      "Adjusted Fuel Economy, Combined 43/57 - real world, CS, Fuel (Gas.Equivalent): {mile/gallon}",
      "Adjusted Electricity Consumption, Combined 43/57 - real world, CD (Wh/mi)"
    )
  )  %>%
  filter(Progress == "Low", #autonomie "low" corresponds to tempo "mid"
         Powertrain %in% tempo_autonm_gcam_tech_mapping$autonomie.tech, 
         Performance == "Base") %>%
  # convert to 2005$ using gdp_deflator(2005, 2021) in latest version of gcamdata (up to 2021, although the values are in 2023$)
  mutate(MSRP = MSRP * 0.7374201) 

# map autonomie to tempo and gcam
Auto_LDV_mapped <- Auto_LDV %>% 
  left_join(tempo_autonm_gcam_tech_mapping, by = c("Powertrain" = "autonomie.tech") ) %>% 
  # five classes in autonomie: 'Compact', 'Midsize', 'Midsize SUV', 'Pickup', 'Small SUV'
  # map them to closest class in input/gcamdata/inst/extdata/energy/OTAQ_trn_data_EMF37.csv 
  mutate(Class = case_when(
    Class %in% c("Compact") ~ "Compact Car",
    Class %in% c("Midsize") ~ "Midsize Car",
    Class %in% c("Midsize SUV") ~ "Large Car and SUV",
    Class %in% c("Pickup") ~ "Large Car",
    Class %in% c("Small SUV") ~ "Large Car and SUV"))

# # check how tempo maps to gcam in vehicle classes
# tempo_passenger_tech_mapping %>% 
#   filter(Tempo_Class %in% c("Compact", "Midsize", "Pickup", "SUV")) %>% 
#   select(Tempo_Class,Size.class) %>% 
#   distinct()

# Add Light Truck & SUV category as as a copy of Large Car 
Auto_LDV_TruckSUV <- Auto_LDV_mapped %>%
  filter(Class == "Large Car") %>%
  mutate(Class = case_when(Class %in% c("Large Car") ~ "Light Truck and SUV"))
  
Auto_LDV_mapped_full <- Auto_LDV_mapped %>%
  bind_rows(Auto_LDV_TruckSUV) %>%
  arrange(Class)
  
# interpolation over years 
# use rule = 2 to write out the data to all model years (to 2100)
HIST_FUT_YEARS<- seq(2005, 2100, 5)
approx_fun <- function(year, value, rule = 1) {
  tryCatch(
    stats::approx(
      as.vector(year),
      value,
      rule = rule,
      xout = year,
      ties = mean
    )$y,
    error = function(e)
      NA
  )
}

# convert intensity to MJ/vkm 
conv_mile_km = 1.60934
conv_Wh_MJ = 0.0036
# mpgge - mile per gallon gas equivalent
conv_gge_MJ = 121.3 # one gallon of gasoline contains approximately 121.3 MJ of energy.

update_table<-as_tibble(Auto_LDV_mapped_full) %>% 
  select(sector, year, size.class = Class, technology = gcam.tech, 
         `Capital costs (purchase)` = MSRP, intensity_mpgge = `Real-world_FE_mpgge`, 
         intensity_Wh_per_mi = `Real-world_elec_cons_Wh/mi`) %>% 
  mutate(fuel = if_else(technology == 'BEV','Electricity',
                        if_else(technology == 'FCEV', 'Hydrogen',
                                if_else(technology %in% c('Liquids', 'Hybrid Liquids'), 'Liquids', 'Natural Gas'))),
         region = 'USA',
         mode = 'LDV_4W',
         `Capital costs (other)` = 0.09*`Capital costs (purchase)`,
         intensity_MJ_per_vkm_other = 1/intensity_mpgge * conv_gge_MJ / conv_mile_km,
         intensity_MJ_per_vkm_elec = intensity_Wh_per_mi * conv_Wh_MJ / conv_mile_km,
         # use intensity_MJ_per_vkm_other for hybrid liquids vehicles for now
         intensity = if_else(fuel == 'Electricity', intensity_MJ_per_vkm_elec, intensity_MJ_per_vkm_other)) %>% 
  select(year, region, sector, mode, size.class, technology, fuel, intensity, `Capital costs (purchase)`, `Capital costs (other)`) %>% 
  gather(variable, value, -year, -region, -sector, -mode, -size.class, -technology, -fuel) %>% 
  group_by(year, region, sector, mode, size.class, technology, fuel, variable) %>% 
  summarise(value = mean(value)) %>% 
  ungroup() %>% 
  group_by(region, sector, mode, size.class, technology, fuel, variable) %>% 
  complete(year = HIST_FUT_YEARS) %>%
  mutate(value = approx_fun(year, value, rule = 2)) %>% 
  ungroup() %>%
  filter(year %in% HIST_FUT_YEARS) %>% 
  mutate(unit = if_else(variable == 'intensity', 'MJ/vkm', '2005$/veh')) %>% 
  select(year, UCD_region = region, UCD_sector = sector, 
         mode, size.class, UCD_technology = technology, UCD_fuel = fuel, 
         variable, value, unit) %>% 
  spread(year, value)

# read in the original file and replace only the lines with updated values
original_table <- as_tibble(read.csv(
  "./inputs/OTAQ_trn_data_EMF37_original.csv",
  skip = 6, blank.lines.skip = TRUE, check.names = F))


final_table<-original_table %>%
  anti_join(
    update_table, by = c('UCD_region', 'UCD_sector', 'mode', 'size.class',
      'UCD_technology', 'UCD_fuel', 'variable', 'unit')) %>% 
  bind_rows(update_table)
  

# Write to CSV, such that the file can be copied directly into gcamdata, which requires the following header/metadata info.
cat("# File: OTAQ_trn_data_EMF37.csv",
    "# Title: MODIFIED OTAQ-specific assumptions to layer into UCD_trn_data_CORE.csv database",
    "# Units: Various",
    "# Comments: MODIFIED intensity and capital cost (other and purchase) based on AUTONOMIE and TEMPO harmonization",
    "# Source: 2023.03.14 Vehicle tables.xlsx workbook. BEV 3-wheeler data from UCD_trn_data_CORE file of CMP 316",
    "# Column types: ccccccccnnnnnnnnnnnnnnnnnnnn",
    "# ----------",
    file = "./outputs/OTAQ_trn_data_EMF37.csv", sep = "\n", append = FALSE)

write_csv(final_table, "./outputs/OTAQ_trn_data_EMF37.csv", append = T, col_names = TRUE)

