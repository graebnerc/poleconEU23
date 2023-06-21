# Define function `get_country_classification()` to map country names 
#  on groups.

base_countries <- c(
  'Austria', 'Belgium', 'Bulgaria', 'Cyprus', 'Czech Republic', 'Germany', 
  'Denmark', 'Estonia', 'Spain', 'Finland', 'France', 'Greece', 'Croatia', 
  'Hungary', 'Ireland', 'Italy', 'Lithuania', 'Luxembourg', 'Latvia', 'Malta', 
  'Netherlands', 'Poland', 'Portugal', 'Romania', 'Sweden', 'Slovenia', 
  'Slovak Republic'
)

base_tibble <- tibble::tibble(
  "country" = base_countries
)
# The JEE classification---------------
ger_group <- "Core"
fra_group <- "Periphery"

jee_classification <- list()
jee_classification[["Germany"]] <- countrycode(
  "Germany", "country.name", "iso2c")
jee_classification[["France"]] <- countrycode(
  "France", "country.name", "iso2c")
jee_classification[["Finance"]] <- countrycode(
  c("Luxembourg", "Netherlands", "Malta", "Ireland"), 
  "country.name", "iso2c")
jee_classification[["Core"]] <- countrycode(
  c("Austria", "Belgium", "Denmark", "Finland", "Sweden"), 
  "country.name", "iso2c")
jee_classification[["Catchup"]] <- countrycode(
  c("Bulgaria", "Romania", "Czech Republic", "Estonia", "Latvia", "Lithuania", 
    "Hungary", "Poland", "Slovenia", "Slovakia", "Croatia"), 
  "country.name", "iso2c")
jee_classification[["Periphery"]] <- countrycode(
  c("Greece", "Italy", "Portugal", "Spain", "Cyprus"), "country.name", "iso2c")

# Geo struc classification-------------

geo_struc_classification <- list()
geo_struc_classification[["Central Europe"]] <- countrycode(
  c("Austria", "Belgium", "Denmark", "Finland", "Germany", "Sweden",
    "Luxembourg", "Netherlands", "Malta", "Ireland", "United Kingdom"), 
  "country.name", "iso2c")
geo_struc_classification[["Eastern Europe"]] <- countrycode(
  c("Bulgaria", "Romania", "Czech Republic", "Estonia", "Latvia", "Lithuania", 
    "Hungary", "Poland", "Slovenia", "Slovakia", "Croatia"), 
  "country.name", "iso2c")
geo_struc_classification[["Southern Europe"]] <- countrycode(
  c("Greece", "France", "Italy", "Portugal", "Spain", "Cyprus"), 
  "country.name", "iso2c")

country_classification <- base_tibble %>% 
  mutate(
    iso2c = countrycode(country, "country.name", "iso2c"),
    iso3c = countrycode(iso2c, "iso2c", "iso3c")
  ) %>% 
  mutate(
  jee = ifelse(
    iso2c %in% jee_classification[["Germany"]], ger_group, ifelse(
      iso2c %in% jee_classification[["France"]], fra_group, ifelse(
        iso2c %in% jee_classification[["Core"]], "Core", ifelse(
          iso2c %in% jee_classification[["Finance"]], "Finance", ifelse(
            iso2c %in% jee_classification[["UK"]], "GBR", ifelse(
              iso2c %in% jee_classification[["Catchup"]], "East", ifelse(
                iso2c %in% jee_classification[["Periphery"]], "Periphery", NA
              )))))))) %>% 
  mutate(
    geo_struc = ifelse(
          iso2c %in% geo_struc_classification[["Central Europe"]], 
          "Central Europe", ifelse(
            iso2c %in% geo_struc_classification[["Eastern Europe"]], 
            "Eastern Europe", ifelse(
                iso2c %in% geo_struc_classification[["Southern Europe"]], 
                "Southern Europe", NA)
            )
          )
    )

# Function definition----------------------------
get_country_classification <- function(x, classification){
  countrycode::countrycode(
      sourcevar = x, 
      origin = "iso3c", 
      destination = classification, 
      custom_dict = as.data.frame(country_classification))
}
get_country_classification <- Vectorize(get_country_classification)

# get_country_classification("BEL", "jee")
