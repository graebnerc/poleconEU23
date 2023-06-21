here::i_am("R/get_data.R")
library(here)
library(countrycode)
library(data.table)
library(WDI)
library(tidyr)
library(dplyr)
library(eurostat)

macro_countries <- c(
  'AT', 'BE', 'BG', 'CY', 'CZ', 'DE', 'DK', 'EE', 'ES', 'FI', 'FR', 'GR', 
  'HR', 'HU', 'IE', 'IT', 'LT', 'LU', 'LV', 'MT', 'NL', 'PL', 'PT', 'RO', 
  'SE', 'SI', 'SK'
)

# GDP data-----------------------------

income_growth <- WDI::WDI(
  indicator = c(
    "gdp_ppp_pc"="NY.GDP.PCAP.PP.CD",
    "GDP_constUSD"="NY.GDP.PCAP.KD",
    "population"="SP.POP.TOTL"
    ),
  country = macro_countries) %>% 
  dplyr::select(-iso3c)
fwrite(income_growth, file = here("data/wdi_gdp.csv"))
# 
# # GDP data-----------------------------
# long_term_gdp_data <- WDI::WDI(
#   indicator = c(
#     "GDP_constUSD"="NY.GDP.PCAP.KD",
#     "GDP_ppp"="NY.GDP.PCAP.PP.CD"),
#   country = macro_countries)
# fwrite(long_term_gdp_data, file = here("data/gdp_longterm.csv"))

# Get spatial data for maps------------
spatial_data_raw <- eurostat::get_eurostat_geospatial(
  output_class = "df",
  resolution = "20",
  nuts_level = "2"
)
fwrite(spatial_data_raw, file = here("data/eurostat_spatial.csv"))

# Get local data for GDP values on maps----------
local_data <- eurostat::get_eurostat(id = "nama_10r_2gdp", time_format = "num")
fwrite(local_data, file = here("data/eurostat_nuts2.csv"))

local_data_hr <- local_data %>%
  filter(nchar(geo)==4) %>%
  eurostat::label_eurostat(
    lang = "en", 
    fix_duplicated = TRUE, 
    code = "geo"
    ) %>%
  mutate(country=countrycode(substr(geo_code, 1,2), "iso2c", "country.name"))
fwrite(local_data_hr, file = here("data/eurostat_nuts2_ready.csv"))
