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
load("cbsa_centroids.RData")

# Create YOY Change
pctchange <- function(x) {
  (x - lag(x, n = 12)) / lag(x, n = 12)
}
lvlchange <- function(x) {
  (x - lag(x, n = 12))
}
zillowVariables <- c(
  "ZHVI",
  "ZORI",
  "MedianDOZ",
  "InventoryForSale",
  "SaleListingRatio",
  "PriceRentRatio"
)

# Zillow CBSA
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

# Zillow ZCTA
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

# NHGIS
nhgis_zcta <- as.data.table(nhgis_zcta)
nhgisVariables <- list(
  "College Grads (% Pop 25 & Over)" = "CollegeGrad",
  "Finance Industries (% Worker)" = "Finance/Insurance/RealEstate",
  "Foreign Born (% Population)" = "Foreign.Born",
  "Gini Inequality Index" = "Gini",
  "Have Computer (% Households)" = "HaveComputer",
  "Have Internet (% Households)" = "HaveInternet",
  "Hispanic (% Population)" = "Hispanic",
  "IT Industry (% Worker)" = "Information",
  "Managerial Occs (% Worker)" = "Management",
  "Median Income (1,000s)" = "MedianIncome",
  "Median Number of Rooms (Housing)" = "MedianRooms",
  "Median Year Built" = "MedianYearBuilt",
  "Owner Occupied (% Housing Units)" = "OwnerOccupied",
  "Per Capita Income (1,000s)" = "PerCapitaIncome",
  "Population (1,000s)" = "Pop",
  "Population Density" = "PopDensity",
  "Travel to Work by Car (% Worker)" = "Car",
  "Unemployment Rate" = "UnempRate",
  "White (% Population)" = "White"
)

# Shapefiles
zcta_map <- st_read("zcta_map.shp")
