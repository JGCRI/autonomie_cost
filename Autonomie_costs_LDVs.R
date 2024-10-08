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

# Read in selected columns from Autonomie MHDT truck cost spreadsheet, for the "Low" case, 
# only include powertrains that are included in GCAM and convert to 2005$, and rename categories
Auto_LDV <- fread("LDV_Autonomie.csv", select =  c("Model Years: {years}", 
                                                   "Vehicle Class: {string}", 
                                                   "Vehicle Powertrain: {string}", 
                                                   "Technology Progress: {string}", 
                                                   "Vehicle Performance Category: {string}", 
                                                   "Vehicle Curb Weight: {Kg}", 
                                                   "Vehicle MSRP {2023$}", 
                                                   #"Levelized Cost of Driving - LCOD: {$/mile}", 
                                                   "Adjusted Fuel Economy, Combined 43/57 - real world, CS, Fuel (Gas.Equivalent): {mile/gallon}", 
                                                   "Adjusted Electricity Consumption, Combined 43/57 - real world, CD (Wh/mi)")) %>%
rename_with(~c("MY", 
               "Class", 
               "Powertrain", 
               "Progress", 
               "Performance", 
               "Curb_weight_kg", 
               "MSRP", 
               #"LCOD_$/mile", 
               "Real-world_FE_mpgge", 
               "Real-world_elec_cons_Wh/mi"), 
            c("Model Years: {years}", 
              "Vehicle Class: {string}", 
              "Vehicle Powertrain: {string}", 
              "Technology Progress: {string}", 
              "Vehicle Performance Category: {string}", 
              "Vehicle Curb Weight: {Kg}", 
              "Vehicle MSRP {2023$}", 
              #"Levelized Cost of Driving - LCOD: {$/mile}", 
              "Adjusted Fuel Economy, Combined 43/57 - real world, CS, Fuel (Gas.Equivalent): {mile/gallon}", 
              "Adjusted Electricity Consumption, Combined 43/57 - real world, CD (Wh/mi)")) %>%
# first rename columns
  filter(Progress == "Low") %>%
  filter(Powertrain == "Conventional SI" | 
           Powertrain == "BEV300" | 
           Powertrain == "FCEV" | 
           Powertrain == "Par HEV SI") %>% # change to Split? 
  filter(Performance == "Base") %>%
  mutate(MSRP = MSRP * gdp_deflator(2005, 2021)) %>% # convert to 2005$
  mutate(Powertrain = case_when(
    Powertrain %in% c("Conventional SI") ~ "Liquids",
    Powertrain %in% c("BEV300") ~ "BEV",
    Powertrain %in% c("FCEV") ~ "Hydrogen",
    Powertrain %in% c("Par HEV SI") ~ "HEV")) %>%
  # pick size classes that best match GCAM classes
  filter(Class == "Compact" |
           Class == "Midsize" | 
           Class == "Midsize SUV") %>%
  mutate(Class = case_when(
    Class %in% c("Midsize SUV") ~ "Large Car",
    Class %in% c("Compact") ~ "Compact",
    Class %in% c("Midsize") ~ "Midsize"))

# Add Light Truck & SUV category as as a copy of Large Car 
Auto_LDV_TruckSUV <- Auto_LDV %>%
  filter(Class == "Large Car") %>%
  mutate(Class = case_when(
    Class %in% c("Large Car") ~ "Light Truck and SUV"))
  
Auto_LDV <- Auto_LDV %>%
  rbind(Auto_LDV_TruckSUV) %>%
  arrange(Class)
  
Auto_LDV


# To do: Create interpolation loop --------------------------------------------
# better: use approx.fun from gcamdata 
# > approx_fun <- function(year, value, rule = 2) {
#tryCatch(stats::approx(as.vector(year), value, rule = rule, xout = year, ties = mean)$y,
#         error = function(e) NA)
#}
#
#We'll use rule = 2 to write out the data to all model years (to 2100)
 

# interpolation years 2020, 2040, 2045:
interpol_years <- c("2020", "2040", "2045") %>%
  as.numeric



# Everything below is just me trying out stuff and can probably deleted if it's not useful:
#make new column that contains powertrain and class
Auto_LDV <- Auto_LDV %>%
mutate(Vehicle = paste(Powertrain, Class)) %>%
  select(MY, Vehicle, MSRP)

interpol_MSRP <- approx(
  x = Auto_LDV$MY,         # Known years
  y = Auto_LDV$MSRP,        # Known values
  xout = interpol_years  # Years to interpolate
) %>%
  as_tibble()


for (i in Auto_LDV$Vehicle) {
interpolated_values <- approx(
  x = Auto_LDV$MY,         # Known years
  y = Auto_LDV$MSRP,
  xout = interpol_years 
) %>%
  as_tibble()
}

df_total <- rbind(i, interpolated_values, x, y)


interpol_powertrain_class_MSRP <- approx(
  x = Auto_LDV$MY,         # Known years
  y = Auto_LDV$MSRP,        # Known values
  xout = interpol_years  # Years to interpolate
) %>%
  as_tibble()

# Example
interpol_BEV_compact <- approx(
  x = BEV_compact$MY,         # Known years
  y = BEV_compact$MSRP,        # Known values
  xout = interpol_years  # Years to interpolate
) %>%
  as_tibble() %>%
  dplyr::rename(MY = x, MSRP = y)



BEV_compact <- Auto_LDV %>%
  filter(Powertrain == "BEV") %>%
  filter(Class == "Compact")

#BEV_trend <- ggplot(data = interpol_BEV, aes(x = year, y = MSRP)) +
#  geom_line() 
#BEV_trend  


# Deselect 2023

#Auto_LDV_wide <- Auto_LDV %>%
#  tidyr::gather(key = "year", value = "value", -Class, -Powertrain, -Progress, -Performance, -Curb_weight_kg, -MSRP, -`LCOD_$/mile`, `Real-world_FE_mpgge`, `Real-world_elec_cons_Wh/mi`)


