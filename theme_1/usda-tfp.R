
# data from https://www.ers.usda.gov/data-products/international-agricultural-productivity/
# machine readable wide form spreadsheet tab

# Description of variables in the USDA, Economic Research Service, International Agricultural Productivity data product		
# 
# Variable Name	Description	Unit
# Order	Dataset code for geographic area	
# FAO	FAO country code	
# ISO3	ISO3 country code	
# Country/territory	Country/territory name	
# Region	Region name	
# Sub-region	Sub-region name	
# Inc I	Income class of country, World Bank 2020	
# Year	Year	
# TFP_Index	Index of agricultural TFP	Index, 2015=100
# Outall_Index	Index of total agricultural output	Index, 2015=100
# Input_Index	Index of total agricultural input	Index, 2015=100
# Land_Index	Index of total agricultural land input	Index, 2015=100
# Labor_Index	Index of total agricultural labor input	Index, 2015=100
# Capital_Index	Index of total agricultural capital input	Index, 2015=100
# Materials_Index	Index of total agricultural materials input	Index, 2015=100
# Outall_Q	Quantity of total agricultural output	$1000, constant 2015 prices
# Outcrop_Q	Quantity of total crop output	$1000, constant 2015 prices
# Outanim_Q	Quantity of total animal output	$1000, constant 2015 prices
# Outfish_Q	Quantity of total aquaculture output	$1000, constant 2015 prices
# Land_Q	Quantity of total agricultural land	1000 hectares, rainfed-cropland-equivalents
# Labor_Q	Quantity of total agricultural labor	1000 persons economically active in agriculture
# Capital_Q	Quantity of total agricultural capital stock	$million, constant 2015 prices
# Machinery_Q	Quantity of total agricultural machinery stock	1000 metric horsepower (CV)
# Livestock_Q	Quantity of total agricultural animal inventories	1000 head of standard livestock units
# Fertilizer_Q	Quantity of total agricultural fertilizer	Metric tons of inorganic N, P, K and organic N
# Feed_Q	Quantity of total agricultural feed	106 Mcal of metabolizable energy
# Cropland_Q	Quantity of total cropland	1000 hectares
# Pasture_Q	Quantity of total permanent pasture	1000 hectares
# IrrigArea_Q	Quantity of total area equipped for irrigation	1000 hectares
# Source: USDA, Economic Research Service, data as of September 2023.		

library(ggplot2)
library(dplyr)
library(aws.ec2metadata)
library(aws.s3)
library(readr)
library(ukfsr)
library(afcolours)
library(here)


usda <- aws.s3::s3read_using(FUN = read_csv,
                            bucket = ukfsr::s3_bucket(),
                            object = "theme_1/input_data/usda-ers-iap-tfp.csv")



tfp_countries <- usda |> 
  mutate(land_partial = (outall_index/land_index) * 100,
         labour_partial = (outall_index/labor_index) * 100,
         capital_partial = (outall_index/capital_index) * 100) |> 
  select(order:tfp_index, land_partial, labour_partial, capital_partial) |> 
  group_by(country_territory) |> 
  mutate(tfp_index = tfp_index/tfp_index[year == 2000] * 100,
         land_partial = land_partial/land_partial[year == 2000] * 100,
         labour_partial = labour_partial/labour_partial[year == 2000] * 100,
         capital_partial = capital_partial/capital_partial[year == 2000] * 100) |> 
  pivot_longer(cols = tfp_index:capital_partial,names_to = "series")

tfp_countries |> 
  filter(country_territory == "World", year >1999) |> 
  ggplot() +
  geom_line(aes(x = year, y = value, colour = series), lwd = 1)



tfp_regions <- usda |> 
  mutate(land_partial = (outall_index/land_index) * 100,
         labour_partial = (outall_index/labor_index) * 100,
         capital_partial = (outall_index/capital_index) * 100) |> 
  select(order:tfp_index, land_partial, labour_partial, capital_partial) |> 
  group_by(region) |> 
  mutate(tfp_index = tfp_index/tfp_index[year == 2000] * 100,
         land_partial = land_partial/land_partial[year == 2000] * 100,
         labour_partial = labour_partial/labour_partial[year == 2000] * 100,
         capital_partial = capital_partial/capital_partial[year == 2000] * 100) |> 
  pivot_longer(cols = tfp_index:capital_partial,names_to = "series")



tfp_regions |> 
  filter(stringr::str_detect(region, "income"), year >1999) |> 
  ggplot() +
  geom_line(aes(x = year, y = value, colour = series), lwd = 1) +
  facet_wrap(vars(region)) 





