# Desc: Shiny Project
# Author: Gary C. Lin

library(shiny)
library(bslib)
library(ggplot2)
library(leaflet)
library(data.table)
library(sf)
library(htmltools)
library(rbokeh)
library(dplyr)
library(biscale)
library(cowplot)
theme_set(theme_bw())

# Load data
load("zillow_cbsa.RData")
load("zillow_zcta.RData")
load("nhgis_zcta.RData")
pctchange <- function(x) {
  (x - lag(x, n = 12)) / lag(x, n = 12)
}
lvlchange <- function(x) {
  (x - lag(x, n = 12))
}
zillowVariables <- colnames(cbsa)[7:12]

cbsa <- cbsa %>%
  group_by(MetroName) %>%
  arrange(MetroName, Date) %>%
  mutate(
    across(
      contains(zillowVariables),
      .fns = list(DPCT = ~ pctchange(.), DLVL = ~ lvlchange(.), LVL = ~.),
      .names = "{fn}_{col}"
    )
  ) %>%
  ungroup()

cbsa <- as.data.table(cbsa)
cbsa <- cbsa[Year >= 2015 & Year <= 2020]

zcta <- zcta %>%
  group_by(ID) %>%
  arrange(ID, Date) %>%
  mutate(
    across(
      contains(zillowVariables),
      .fns = list(DPCT = ~ pctchange(.), DLVL = ~ lvlchange(.), LVL = ~.),
      .names = "{fn}_{col}"
    )
  ) %>%
  ungroup()

zcta <- as.data.table(zcta)
zcta <- zcta[Year >= 2015 & Year <= 2020]
zcta <- zcta[!ID %in% c(12853, 2663, 4417)]
nhgis_zcta <- as.data.table(nhgis_zcta)
nhgis_zcta <- nhgis_zcta[!ID %in% c(12853, 2663, 4417)]

# National Trends Panel
zillowVariables <- colnames(cbsa)[7:12]
nhgisVariables <- list(
  "College Graduates (% Population 25 and Over)" = "CollegeGrad",
  "Finance, Insurance, & Real Estate Industries" = "Finance/Insurance/RealEstate",
  "Foreign Born (% Population)" = "Foreign.Born",
  "Gini Inequality Index" = "Gini",
  "Have Computer (% Households)" = "HaveComputer",
  "Have Internet (% Households)" = "HaveInternet",
  "Hispanic (% Population)" = "Hispanic",
  "Information Industry (% Worker)" = "Information",
  "Managerial Occs (% Worker)" = "Management",
  "Median Income" = "MedianIncome",
  "Median Number of Rooms (Housing)" = "MedianRooms",
  "Median Year Built" = "MedianYearBuilt",
  "Owner Occupied (% Housing Units)" = "OwnerOccupied",
  "Per Capita Income" = "PerCapitaIncome",
  "Population" = "Pop",
  "Population Density" = "PopDensity",
  "Travel to Work by Car (% Worker)" = "Car",
  "Unemployment Rate" = "UnempRate",
  "White (% Population)" = "White"
)

# Shapefiles
zcta_map <- st_read("zcta_map.shp")
zcta_map <- zcta_map %>% rename(ID = geoid)
