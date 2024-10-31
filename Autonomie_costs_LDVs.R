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

# ---- PART 1 extract energy intensity and capital costs in Autonomie ----
# that are mapped to TEMPO Mid scenario 
# we will use these values for GCAM as well 

# Read in mapping file ----
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
Auto_LDV0 <- fread(
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
  )  

gdp_deflator <- function(year, base_year) {
  # This time series is the BEA "A191RD3A086NBEA" product
  # Downloaded Oct 18, 2024 from https://fred.stlouisfed.org/series/A191RD3A086NBEA
  # FRED data was re-based; extend the index to 2023 using annual rates
  gdp_years <- 1929:2023
  gdp <- c(9.896, 9.535, 8.555, 7.553, 7.345, 7.749, 7.908, 8.001, 8.347,
           8.109, 8.033, 8.131, 8.68, 9.369, 9.795, 10.027, 10.288, 11.618,
           12.887, 13.605, 13.581, 13.745, 14.716, 14.972, 15.157, 15.298,
           15.559, 16.091, 16.625, 17.001, 17.237, 17.476, 17.669, 17.886,
           18.088, 18.366, 18.702, 19.227, 19.786, 20.627, 21.642, 22.784,
           23.941, 24.978, 26.337, 28.703, 31.361, 33.083, 35.135, 37.602,
           40.706, 44.377, 48.52, 51.53, 53.565, 55.466, 57.24, 58.395,
           59.885, 61.982, 64.392, 66.773, 68.996, 70.569, 72.248, 73.785,
           75.324, 76.699, 78.012, 78.859, 80.065, 81.887, 83.754, 85.039,
           86.735, 89.12, 91.988, 94.814, 97.337, 99.246, 100, 101.221,
           103.311, 105.214, 106.913, 108.828, 109.998, 111.445, 113.545,
           116.311, 118.339, 119.766, 124.743, 133.636, 138.444)
  names(gdp) <- gdp_years
  
  # assert_that(all(year %in% gdp_years))
  # assert_that(all(base_year %in% gdp_years))
  
  as.vector(unlist(gdp[as.character(year)] / gdp[as.character(base_year)]))
}

Auto_LDV<- Auto_LDV0 %>%
  filter(Progress == "Low", #autonomie "low" corresponds to tempo "mid"
         Powertrain %in% tempo_autonm_gcam_tech_mapping$autonomie.tech, 
         Performance == "Base") %>%
  # convert to 2005$
  # in latest version of gcamdata, gdp_deflator is up to 2021, but the values are in 2023$
  # we updated the gcamdata gdp_deflator() above to convert 2023 USD to 2005 USD
  mutate(MSRP = MSRP * gdp_deflator(2005,2023)) 

# map autonomie to tempo and gcam
Auto_LDV_mapped <- Auto_LDV %>% 
  left_join(tempo_autonm_gcam_tech_mapping, by = c("Powertrain" = "autonomie.tech") ) %>% 
  # five classes in autonomie: 'Compact', 'Midsize', 'Midsize SUV', 'Pickup', 'Small SUV'
  # map them to closest class in input/gcamdata/inst/extdata/energy/OTAQ_trn_data_EMF37.csv 
  mutate(Class = case_when(
    Class %in% c("Compact") ~ "Compact Car",
    Class %in% c("Midsize") ~ "Midsize Car",
    Class %in% c("Midsize SUV") ~ "Large Car",
    Class %in% c("Pickup") ~ "Large Car",
    Class %in% c("Small SUV") ~ "Large Car"))

# # check how tempo maps to gcam in vehicle classes
# tempo_passenger_tech_mapping %>%
#   filter(Sector == 'Passenger',
#          Tempo_Class %in% c("Compact", "Midsize", "Pickup", "SUV")) %>%
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


# read in tempo's atb table which is derived from Autonomie and is the direct input to tempo
# however, the atb table only starts from 2023, so we will do an ad-hoc of autonomie table and atb table
# to include more historical years and also to match closer to the atb table (closer to tempo inputs)
tempo_table <- as_tibble(read.csv(
  "./inputs/atb_vehicles.csv",
  skip = 0, blank.lines.skip = TRUE, check.names = F))

tempo_table %>% 
  filter(vehicle_weight_category == 'Light Duty') %>% 
  select(scenario,year, vehicle_class,vehicle_detail,metric,value) %>% 
  rename(size.class = vehicle_class) %>% 
  mutate(size.class = case_when(
    size.class %in% c("Compact") ~ "Compact Car",
    size.class %in% c("Midsize") ~ "Midsize Car",
    size.class %in% c("Midsize SUV") ~ "Large Car",
    size.class %in% c("Pickup") ~ "Large Car",
    size.class %in% c("Small SUV") ~ "Large Car")) %>% 
  filter(vehicle_detail %in% c('Battery Electric Vehicle (200-mile range)',
                               'Battery Electric Vehicle (300-mile range)',
                               'Battery Electric Vehicle (400-mile range)',
                               'Hydrogen Fuel Cell Electric Vehicle',
                               'Gasoline Power Split Hybrid Electric Vehicle',
                               'Gasoline ICE Vehicle (spark ignition with turbo)',
                               'Compressed Natural Gas Conventional Vehicle')) %>% 
  rename(technology = vehicle_detail) %>% 
  mutate(technology = case_when(
    grepl('Battery Electric Vehicle',technology) ~ 'BEV',
    technology == 'Hydrogen Fuel Cell Electric Vehicle' ~ 'FCEV',
    technology == 'Gasoline Power Split Hybrid Electric Vehicle' ~ 'Hybrid Liquids',
    technology == 'Gasoline ICE Vehicle (spark ignition with turbo)' ~ 'Liquids',
    technology == 'Compressed Natural Gas Conventional Vehicle' ~ 'NG'
  )) %>% 
  filter(metric %in% c('Modeled Vehicle Price (2022$)','Fuel Economy (mi/gge)')) %>% 
  rename(variable = metric) %>% 
  mutate(variable = case_when(
    variable == 'Modeled Vehicle Price (2022$)' ~ 'Capital costs (purchase)',
    variable == 'Fuel Economy (mi/gge)' ~ 'intensity'
  )) %>% 
  group_by(scenario,year,size.class,technology,variable) %>% 
  summarise(value = mean(value)) %>% 
  ungroup() %>% 
  spread(variable, value) %>% 
  # convert 2022$ to 2005$; convert mi/gge to MJ/vkm
  mutate(`Capital costs (purchase)` = `Capital costs (purchase)`*gdp_deflator(2005,2022),
         intensity = 1/intensity * conv_gge_MJ / conv_mile_km) %>% 
  gather(variable, value, -scenario, -year, -size.class, -technology)->tempo_table_mapped

# derive values based on Autonomie table
tempo_mid_autonomie<-as_tibble(Auto_LDV_mapped_full) %>% 
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
  ungroup() 

# compare autonomie derived values to tempo atb table
tempo_mid_autonomie %>% 
  left_join(tempo_table_mapped %>% 
              filter(scenario == 'Mid') %>% 
              select(-scenario), by = c('size.class','technology','variable','year')) %>% 
  filter(!is.na(value.y)) %>% 
  rename(Autonomie = value.x,
         Tempo = value.y) %>% 
  gather(source, value, -region, -sector, -mode, -size.class, -technology, -fuel,
         -variable, -year)->compare_tempo_autonomie

ggplot(compare_tempo_autonomie, aes(x = year, y = value, linetype = source, color = size.class))+
  geom_line()+
  facet_grid(rows = vars(variable), cols = vars(technology), scales = 'free_y')+
  theme_bw()
ggsave('figs/compare_tempo_autonomie_mid.png', width = 10, height = 6, units = 'in')
  
# we will use tempo's value for periods available (2023 and forward) and
# adjust autonomie values based on the comparison for other periods (2005-2023)
# both have values in 2023, which is the connecting year of the two sources.
# so we will calculate the adjustment ratio based on values in 2023
compare_tempo_autonomie %>% 
  filter(year == 2023) %>% 
  spread(source, value) %>% 
  mutate(adj.ratio = Tempo/Autonomie) %>% 
  select(-year, -Autonomie, -Tempo)->autonomie_adj_multiplier

autonomie_adj_multiplier %>% 
  filter(size.class == 'Large Car') %>% 
  mutate(size.class = 'Light Truck and SUV') %>% 
  bind_rows(autonomie_adj_multiplier)->autonomie_adj_multiplier

compare_tempo_autonomie %>% 
  filter(size.class == 'Large Car') %>% 
  mutate(size.class = 'Light Truck and SUV') %>% 
  bind_rows(compare_tempo_autonomie)->compare_tempo_autonomie

tempo_mid<-tempo_mid_autonomie %>% 
  filter(year<=2023,
         variable != 'Capital costs (other)') %>% 
  left_join_error_no_match(autonomie_adj_multiplier, 
                           by = c('region','sector','mode','size.class',
                                  'technology','fuel','variable')) %>% 
  mutate(value = value*adj.ratio) %>% 
  select(-adj.ratio) %>% 
  bind_rows(compare_tempo_autonomie %>% 
              filter(source == 'Tempo', year != 2023) %>% 
              select(-source)) %>% 
  filter(year %in% HIST_FUT_YEARS) %>%
  mutate(unit = if_else(variable == 'intensity', 'MJ/vkm', '2005$/veh')) %>%
  select(year, UCD_region = region, UCD_sector = sector,
         mode, size.class, UCD_technology = technology, UCD_fuel = fuel,
         variable, value, unit)

ggplot(tempo_mid %>% filter(size.class != 'Light Truck and SUV'), 
       aes(x = year, y = value, color = size.class))+
  geom_line()+
  facet_grid(rows = vars(variable), cols = vars(UCD_technology), scales = 'free_y')+
  theme_bw()
ggsave('figs/tempo_extended_mid.png', width = 12, height = 6, units = 'in')

update_table<- tempo_mid %>%
  bind_rows(tempo_mid %>% 
              filter(variable == 'Capital costs (purchase)') %>% 
              mutate(value = value*0.09,
                     variable = 'Capital costs (other)')) %>% 
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

# ---- PART 2 Calculate scenario specific adjustment factors ----
# Autonomie 'low' is used for Tempo 'Mid' scenario
# which is used for our base values in GCAM's 'MidTech' scenario 
# while intensity and costs in other scenarios will be adjusted using 'adj_factor' (multiplier)
# Note Autonomie 'high' is used for both Tempo 'High' and 'Net-zero' scenarios 

gcam_size_class_mapping <- as_tibble(read.csv(
  "mappings/UCD_size_class_revisions.csv",
  skip = 7, blank.lines.skip = TRUE, check.names = F)) %>% 
  filter(UCD_region == 'USA')

# gcam_tech_mapping <- as_tibble(read.csv(
#   "mappings/UCD_techs.csv",
#   skip = 7, blank.lines.skip = TRUE, check.names = F)) 

Auto_LDV<- Auto_LDV0 %>%
  filter(Progress == "High", #autonomie "high" corresponds to tempo "advanced"
         Powertrain %in% tempo_autonm_gcam_tech_mapping$autonomie.tech, 
         Performance == "Base") %>%
  mutate(MSRP = MSRP * gdp_deflator(2005,2023)) 

# map autonomie to tempo and gcam
Auto_LDV_mapped <- Auto_LDV %>% 
  left_join(tempo_autonm_gcam_tech_mapping, by = c("Powertrain" = "autonomie.tech") ) %>% 
  # five classes in autonomie: 'Compact', 'Midsize', 'Midsize SUV', 'Pickup', 'Small SUV'
  # map them to closest class in input/gcamdata/inst/extdata/energy/OTAQ_trn_data_EMF37.csv 
  mutate(Class = case_when(
    Class %in% c("Compact") ~ "Compact Car",
    Class %in% c("Midsize") ~ "Midsize Car",
    Class %in% c("Midsize SUV") ~ "Large Car",
    Class %in% c("Pickup") ~ "Large Car",
    Class %in% c("Small SUV") ~ "Large Car"))

# Add Light Truck & SUV category as as a copy of Large Car 
Auto_LDV_TruckSUV <- Auto_LDV_mapped %>%
  filter(Class == "Large Car") %>%
  mutate(Class = case_when(Class %in% c("Large Car") ~ "Light Truck and SUV"))

Auto_LDV_mapped_full <- Auto_LDV_mapped %>%
  bind_rows(Auto_LDV_TruckSUV) %>%
  arrange(Class)

tempo_adv_autonomie<-as_tibble(Auto_LDV_mapped_full) %>% 
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
  ungroup()

tempo_adv_autonomie %>% 
  rename(adv = value) %>% 
  left_join_error_no_match(tempo_mid_autonomie %>% 
              rename(mid = value), 
            by = c('year','region','sector','mode',
                   'size.class','technology','fuel',
                   'variable')) %>%
  left_join_error_no_match(gcam_size_class_mapping, 
                           by = c("region" = "UCD_region", "mode", "size.class")) %>% 
  select(subsector = rev_size.class, technology, variable, value = year, mid, adv) %>% 
  mutate(value = as.character(value),
         scenario = 'HiTech',
         supplysector = 'trn_pass_road_LDV_4W') %>% 
  group_by(scenario, supplysector, subsector, technology, value, variable) %>% 
  summarise(mid.avg = mean(mid),
            adv.avg = mean(adv)) %>% 
  ungroup() %>% 
  mutate(adj_factor = round(adv.avg/mid.avg,3),
         rule_number = NA_integer_) %>% 
  filter(variable %in% c('intensity','Capital costs (purchase)')) %>% 
  mutate(variable = if_else(variable == 'intensity', 'coefficient','input.cost')) %>% 
  select(-mid.avg, -adv.avg)->adj_table_autonomie

#adj factor based on tempo atb table for hi-tech and stated policy scenarios
tempo_table_mapped %>% 
  left_join_error_no_match(gcam_size_class_mapping %>% 
                             filter(UCD_region == 'USA',
                                    mode == 'LDV_4W'), 
                           by = c("size.class")) %>% 
  select(scenario, year, subsector = rev_size.class, technology, variable, value) %>% 
  mutate(supplysector = 'trn_pass_road_LDV_4W') %>% 
  group_by(scenario, year, supplysector, subsector, technology, variable) %>% 
  summarise(value = mean(value)) %>% 
  ungroup() %>% 
  spread(scenario,value) %>% 
  mutate(adj_factor_hi = round(Advanced/Mid,3),
         adj_factor_sp = round(Conservative/Mid,3)) %>% 
  select(-Mid, -Conservative, -Advanced) %>% 
  mutate(variable = if_else(variable == 'intensity', 'coefficient','input.cost'))->adj_table_tempo

adj_table_autonomie %>% 
  mutate(year = as.integer(value)) %>% 
  inner_join(adj_table_tempo, 
             by = c('supplysector','year','subsector','technology','variable')) %>% 
  select(subsector,technology,year,variable,Autonomie = adj_factor, Tempo = adj_factor_hi) %>% 
  gather(source, value, -subsector, -technology, -year, -variable)->compare_adj_factor 


ggplot(compare_adj_factor, 
       aes(x = year, y = value, linetype = source, color = subsector))+
  geom_line()+
  facet_grid(rows = vars(variable), cols = vars(technology))+
  theme_bw()
ggsave('figs/compare_adj_factor_hi.png', width = 10, height = 6, units = 'in')

# we will just use the tempo table to calculate the adjustment factors in the future periods
adj_table_tempo %>% 
  rename(IRA = adj_factor_sp,
         HiTech = adj_factor_hi) %>% 
  gather(scenario, adj_factor, -year, -supplysector, -subsector, -technology, -variable) %>% 
  filter(year != 2023) %>% 
  mutate(rule_number = NA_integer_) %>% 
  select(scenario, supplysector, subsector, technology, 
         variable, value = year, adj_factor, rule_number) %>% 
  mutate(value = as.character(value))->update_table

# read in the original file and add the updated table
original_table <- as_tibble(read.csv(
  "./inputs/DECARB_trn_scenarios_original.csv",
  skip = 7, blank.lines.skip = TRUE, check.names = F))


final_table <-original_table %>%
  bind_rows(update_table)

# Write to CSV, such that the file can be copied directly into gcamdata, which requires the following header/metadata info.
cat("# File: DECARB_trn_scenarios.csv",
    "# Title: Parameter adjustments to define the transportation scenarios",
    "# Units: Various",
    "# Comments: This is intended as a single control table for scenario differentiation",
    "# Source: harmonization exercise with TEMPO",
    "# Column types: ccccccni",
    "# ----------",
    file = "./outputs/DECARB_trn_scenarios.csv", sep = "\n", append = FALSE)

write_csv(final_table, "./outputs/DECARB_trn_scenarios.csv", append = T, col_names = TRUE)





