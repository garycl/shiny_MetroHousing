# Desc: Shiny Project
# Author: Gary C. Lin

rm(list = ls())
library(shiny)
library(bslib)
library(ggplot2)
library(leaflet)
library(dplyr)
library(broom)
library(rmapshaper)
library(lubridate)
library(data.table)
library(sf)
library(htmltools)
library(rbokeh)

theme_set(theme_bw())

# Load data
setwd('/Users/Gary/Dropbox/Projects/shiny_project/app/')

# Load data
load('zillow_cbsa.RData')
load('zillow_zcta.RData')
#load('nyt.RData')
#load('alced.RData')
pctchange <- function(x)
  (x - lag(x, n = 12)) / lag(x, n = 12)
lvlchange <- function(x)
  (x - lag(x, n = 12))
zillowVariables = colnames(cbsa)[7:12]

cbsa = cbsa %>%
  group_by(MetroName) %>%
  arrange(MetroName, Date) %>%
  mutate(
    across(
      contains(zillowVariables),
      .fns = list(DPCT = ~pctchange(.), DLVL = ~lvlchange(.), LVL = ~.),
      .names = "{fn}_{col}"
    )
  ) %>%
  ungroup()

cbsa = as.data.table(cbsa)
cbsa = cbsa[, ID := MetroName]
cbsa = cbsa[Year >= 2015 & Year <= 2020]

zcta = zcta %>%
  group_by(ID) %>%
  arrange(ID, Date) %>%
  mutate(
    across(
      contains(zillowVariables),
      .fns = list(DPCT = ~pctchange(.), DLVL = ~lvlchange(.), LVL = ~.),
      .names = "{fn}_{col}"
    )
  ) %>%
  ungroup()
  
zcta = as.data.table(zcta)
zcta = zcta[Year >= 2018 & Year <= 2020]
#cbsa = cbsa[size_rank<=50,]
zcta = zcta[PopRank <= 50, ]
zcta = zcta[ID != 12853]

# National Trends Panel
zillowVariables = colnames(cbsa)[7:12]
nhgisVariables = list(
  'College Graduates (% Population 25 and Over)' = 'CollegeGrad',
  'Finance, Insurance, & Real Estate Industries' = 'Finance/Insurance/RealEstate',
  'Foreign Born (% Population)' = 'Foreign.Born',
  'Gini Inequality Index' = 'Gini',
  'Have Computer (% Households)' = 'HaveComputer',
  'Have Internet (% Households)' = 'HaveInternet',
  'Hispanic (% Population)' = 'Hispanic',
  'Information Industry (% Worker)' = 'Information',
  'Managerial Occs (% Worker)' = 'Management',
  'Median Income' = 'Median Income',
  'Median Number of Rooms (Housing)' = 'MedianRooms',
  'Median Year Built' = 'MedianYearBuilt',
  'Owner Occupied (% Housing Units)' = 'OwnerOccupied',
  'Per Capita Income' = 'PerCapitaIncome',
  'Population' = 'Pop',
  'Population Density' = 'PopDensity',
  'Travel to Work by Car (% Worker)' = 'Car',
  'Unemployment Rate' = 'UnempRate',
  'White (% Population)' = 'White'
)

# Analysis Sample
#sample_metro = zcta[year==2019 & is.na(dl_zhvi_ratio)==0,
#                    .N, 
#                    by = .(metro_name,date)][N>=30]
#sample_metro = sample_metro[, .N, by = metro_name][N==12]
#cbsa = merge(cbsa, sample_metro, by.x = 'ID', by.y = 'metro_name')
#zcta = merge(zcta, sample_metro, by = 'metro_name')

# Load ZCTA shapefile/construct ZCTA shape file if it does not exist
if (!file.exists('zcta_map.shp')) {
  MetroName = zcta %>% select(ID, geoid=ID, MetroName) %>% distinct()
  zcta_map = st_read(paste0('../data/shapefiles/zcta'))
  zcta_map = zcta_map %>%
    select(starts_with('GEOID'))
  colnames(zcta_map) = c('geoid','geometry')
  zcta_map$geoid = as.numeric(zcta_map$geoid)
  zcta_map = inner_join(zcta_map, MetroName, by = 'geoid', all = TRUE)
  zcta_map %>% filter(is.na('ID') == 0)
  zcta_map = st_transform(
    zcta_map,
    "+proj=longlat +datum=WGS84"
  )
  zcta_map = ms_simplify(zcta_map, keep = 0.01, keep_shapes = T)
  st_write(zcta_map, 'zcta_map.shp')
}
zcta_map = st_read('zcta_map.shp')