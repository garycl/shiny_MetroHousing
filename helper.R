rm(list = ls())
library(tidyr)
library(dplyr)
library(data.table)
library(geosphere)
library(rmapshaper)
library(sf)

setwd("/Users/Gary/Dropbox/Projects/shiny_MetroHousing/data/csv")

# NHGIS FILES ####

# Read File Function
read_nhgis <- function(file) {
  colnames <- names(fread(file,
    header = TRUE,
    nrow = 1
  ))
  nhgis <- fread(file,
    header = FALSE,
    skip = 2,
    col.names = colnames
  )
  return(nhgis)
}

# Prep NHGIS File Function By
prep_nhgis <- function(area) {
  # NHGIS244
  file244_path <- paste0("nhgis/nhgis244", area, ".csv")
  nhgis244 <- as.data.table(read_nhgis(file244_path))
  colnames <- c(
    "Pop",
    "White",
    "Black",
    "NativeAmerican",
    "Asian",
    "PacificIslander",
    "OtherRaces",
    "Multiple",
    "Hispanic",
    "Car",
    "PublicTransport",
    "OtherTransport",
    "Bike/Walk",
    "WorkFromHome",
    "LessThanHighSchool",
    "HighSchool",
    "SomeCollege",
    "CollegeGrad",
    "MedianIncome",
    "PerCapitaIncome",
    "UnempRate",
    "OwnerOccupied",
    "MedianRooms",
    "MedianYearBuilt",
    "HaveInternet",
    "HaveComputer"
  )
  nhgis244[, (colnames) :=
    list(
      ALUBE001,
      ALUCE002 / ALUCE001,
      ALUCE003 / ALUCE001,
      ALUCE004 / ALUCE001,
      ALUCE005 / ALUCE001,
      ALUCE006 / ALUCE001,
      ALUCE007 / ALUCE001,
      (ALUCE008 + ALUCE009 + ALUCE010) / ALUCE001,
      ALULE003 / ALULE001,
      ALU1E002 / ALU1E001,
      ALU1E010 / ALU1E001,
      (ALU1E016 + ALU1E017 + ALU1E020) / ALU1E001,
      (ALU1E018 + ALU1E019) / ALU1E001,
      ALU1E021 / ALU1E001,
      (
        ALWGE002 + ALWGE003 + ALWGE004 + ALWGE005 + ALWGE006 + ALWGE007 +
          ALWGE008 + ALWGE009 + ALWGE010 + ALWGE011 + ALWGE012 + ALWGE013 +
          ALWGE014 + ALWGE015 + ALWGE016
      ) / ALWGE001,
      (ALWGE017 + ALWGE018) / ALWGE001,
      (ALWGE019 + ALWGE020 + ALWGE021) / ALWGE001,
      (ALWGE022 + ALWGE023 + ALWGE024 + ALWGE025) / ALWGE001,
      ALW1E001,
      ALX5E001,
      ALY3E005 / ALY3E002,
      ALZLE002 / ALZLE001,
      ALZ5E001,
      AL0EE001,
      AL1YE002 / AL1YE001,
      AL1ZE002 / AL1ZE001
    )]

  if (area == "cbsa") {
    colsSelect <- c("NAME", "CBSAA", colnames)
    nhgis244 <- nhgis244[, .SD, .SDcols = colsSelect]
    nhgis244$State <- gsub(".*, ", "", nhgis244$NAME)
    nhgis244$State <- substr(nhgis244$State, 1, 2)
    nhgis244$NAME[nhgis244$NAME == "Bridgeport-Stamford-Norwalk, CT Metro Area"] <- "Stamford, CT"
    nhgis244$NAME[nhgis244$NAME == "Deltona-Daytona Beach-Ormond Beach, FL Metro Area"] <- "Daytona Beach, FL"
    nhgis244$NAME[nhgis244$NAME == "Palm Bay-Melbourne-Titusville, FL Metro Area"] <- "Melbourne, FL"
    nhgis244$NAME[nhgis244$NAME == "Oxnard-Thousand Oaks-Ventura, CA Metro Area"] <- "Ventura, CA"
    nhgis244$NAME[nhgis244$NAME == "Cape Coral-Fort Myers, FL Metro Area"] <- "Fort Myers, FL"
    nhgis244$NAME <- gsub("[-/,].*", "", nhgis244$NAME)
    nhgis244 <- nhgis244[, c("MetroName", "ID", "NAME", "CBSAA") :=
      list(NAME, CBSAA, NULL, NULL)]
  } else {
    colsSelect <- c("ZCTA5A", colnames)
    nhgis244 <- nhgis244[, .SD, .SDcols = colsSelect]
    nhgis244 <- nhgis244[, c("ID", "ZCTA5A") := list(ZCTA5A, NULL)]
  }

  # NHGIS245
  file245_path <- paste0("nhgis/nhgis245", area, ".csv")
  nhgis245 <- as.data.table(read_nhgis(file245_path))
  colnames <- c(
    "Foreign.Born",
    "Gini",
    "Agriculture",
    "Construction",
    "Manufacturing",
    "WholesaleTrade",
    "RetailTrade",
    "Transportation/Utilities",
    "Information",
    "Finance/Insurance/RealEstate",
    "Professional",
    "Education/Healthcare",
    "Arts/Entertainment",
    "OtherServices",
    "PublicAdmin",
    "Management",
    "Service",
    "Sales/Office",
    "NaturalResources",
    "Production"
  )
  nhgis245[, (colnames) :=
    list(
      AL5ME003 / AL5ME001,
      AMEME001,
      AMHRE002 / AMHRE001,
      AMHRE003 / AMHRE001,
      AMHRE004 / AMHRE001,
      AMHRE005 / AMHRE001,
      AMHRE006 / AMHRE001,
      AMHRE007 / AMHRE001,
      AMHRE008 / AMHRE001,
      AMHRE009 / AMHRE001,
      AMHRE010 / AMHRE001,
      AMHRE011 / AMHRE001,
      AMHRE012 / AMHRE001,
      AMHRE013 / AMHRE001,
      AMHRE014 / AMHRE001,
      AMHRE015 / AMHRE001,
      AMHRE029 / AMHRE001,
      AMHRE043 / AMHRE001,
      AMHRE057 / AMHRE001,
      AMHRE071 / AMHRE001
    )]
  if (area == "cbsa") {
    colsSelect <- c("NAME", "CBSAA", colnames)
    nhgis245 <- nhgis245[, .SD, .SDcols = colsSelect]
    nhgis245$State <- gsub(".*, ", "", nhgis245$NAME)
    nhgis245$State <- substr(nhgis245$State, 1, 2)
    nhgis245$NAME[nhgis245$NAME == "Bridgeport-Stamford-Norwalk, CT Metro Area"] <- "Stamford, CT"
    nhgis245$NAME[nhgis245$NAME == "Deltona-Daytona Beach-Ormond Beach, FL Metro Area"] <- "Daytona Beach, FL"
    nhgis245$NAME[nhgis245$NAME == "Palm Bay-Melbourne-Titusville, FL Metro Area"] <- "Melbourne, FL"
    nhgis245$NAME[nhgis245$NAME == "Oxnard-Thousand Oaks-Ventura, CA Metro Area"] <- "Ventura, CA"
    nhgis245$NAME[nhgis245$NAME == "Cape Coral-Fort Myers, FL Metro Area"] <- "Fort Myers, FL"
    nhgis245$NAME <- gsub("[-/,].*", "", nhgis245$NAME)
    nhgis245 <- nhgis245[, c("MetroName", "ID", "NAME", "CBSAA") :=
      list(NAME, CBSAA, NULL, NULL)]
    nhgis <- merge(nhgis244, nhgis245, by = c("ID", "MetroName", "State"))
  } else {
    colsSelect <- c("ZCTA5A", colnames)
    nhgis245 <- nhgis245[, .SD, .SDcols = colsSelect]
    nhgis245 <- nhgis245[, c("ID", "ZCTA5A") := list(ZCTA5A, NULL)]
    nhgis <- merge(nhgis244, nhgis245, by = "ID")
  }

  # Latitude/Longitude/Land Area
  gaz_file_path <- paste0("xwalks/gaz_", area, "_2020.txt")
  gaz_data <- fread(gaz_file_path)
  colnames(gaz_data) <- tolower(colnames(gaz_data))
  gaz_data <- gaz_data %>%
    select(geoid, aland_sqmi, intptlat, intptlong) %>%
    rename(
      ID = geoid,
      Longitude = intptlong,
      Latitude = intptlat,
      LandSqMi = aland_sqmi
    )
  nhgis <- merge(nhgis, gaz_data, by = "ID")
  nhgis <- nhgis[, lapply(.SD, function(x) {
    ifelse(is.nan(x), NA, x)
  })]
  # nhgis = nhgis[rowSums(is.na(nhgis) == 1) == 0,]
  nhgis[, PopDensity := Pop / LandSqMi]
}

# Produce data files
areas <- c("cbsa", "zcta")
for (area in areas) {
  nhgis <- prep_nhgis(area)
  nhgis <- as.data.table(nhgis)
  assign(paste0("nhgis_", area), nhgis)
}

# Zillow Data ####

# Read and Prep CSV Function
prep_zillow <- function(file) {
  zillow <- fread(paste0("zillow/", file, ".csv"))
  if ("Metro" %in% colnames(zillow)) {
    zillow$ID <- zillow$RegionName
    zillow$Metro[zillow$Metro == "Bridgeport-Stamford-Norwalk"] <- "Stamford"
    zillow$Metro[zillow$Metro == "Deltona-Daytona Beach-Ormond Beach"] <- "Daytona Beach"
    zillow$Metro[zillow$Metro == "Palm Bay-Melbourne-Titusville"] <- "Melbourne"
    zillow$Metro[zillow$Metro == "Oxnard-Thousand Oaks-Ventura"] <- "Ventura"
    zillow$Metro[zillow$Metro == "Cape Coral-Fort Myers"] <- "Fort Myers"
    zillow$MetroName <- gsub("[-/,].*", "", zillow$Metro)
    zillow$State <- zillow$StateName
  } else if ("MsaName" %in% colnames(zillow)) {
    zillow$ID <- zillow$RegionName
    zillow$MetroName <- gsub("[-/,].*", "", zillow$MsaName)
    zillow$State <- gsub(".*, ", "", zillow$MsaName)
  } else {
    zillow$ID <- gsub("[-/,].*", "", zillow$RegionName)
    zillow$State <- gsub(".*, ", "", zillow$RegionName)
    zillow$PopRank <- zillow$SizeRank
  }

  cols_keep <- grep("^\\d+", colnames(zillow)) # columns to keep
  zillow <- zillow %>%
    group_by(ID, State) %>%
    select(-c(1:min(cols_keep) - 1)) %>%
    pivot_longer(
      cols = starts_with(c("1", "2")),
      names_to = "date",
      values_to = gsub("(Metro\\_)|(Zip\\_)", "", file)
    )

  zillow$Year <- as.numeric(substr(zillow$date, 1, 4))
  zillow$Month <- as.numeric(substr(zillow$date, 6, 7))
  # zillow = zillow[zillow$year>=2018 & zillow$year<=2020, ]
  # zillow = zillow %>% rename(-date)
  return(zillow)
}

# Metro List
df_cbsa <- data.frame()
metro_list <- c(
  "Metro_ZHVI",
  "Metro_ZORI",
  "Metro_med_doz_pending",
  "Metro_median_sale_price",
  "Metro_invt_fs",
  "Metro_mlp"
)
for (metro in metro_list) {
  if (nrow(df_cbsa) == 0) {
    df_cbsa <- prep_zillow(metro)
    df_cbsa <- df_cbsa %>% rename(Date = date)
  }
  else {
    df_cbsa <- merge(df_cbsa,
      prep_zillow(metro),
      by = c("ID", "State", "Year", "Month", "PopRank"),
      all = TRUE
    )
    df_cbsa <- df_cbsa %>% select(-date)
  }
}

# Further clean up
df_cbsa <- df_cbsa %>%
  filter(PopRank <= 100) %>%
  rename(
    MetroName = ID,
    MedianDOZ = med_doz_pending,
    MedianSalePrice = median_sale_price,
    MedianListingPrice = mlp,
    InventoryForSale = invt_fs
  ) %>%
  mutate(
    MedianDOZ = MedianDOZ,
    InventoryForSale = InventoryForSale
  )

# Create CBSA data
cbsa <- inner_join(df_cbsa, nhgis_cbsa, by = c("MetroName", "State"))
cbsa$Date <- as.Date(cbsa$Date, "%Y-%m-%d")
cbsa <- cbsa %>%
  group_by(ID) %>%
  arrange(ID, Date) %>%
  ungroup()
save(cbsa, file = "../../app/zillow_cbsa.RData")

# Clean Zillow ZCTA Data ####
df_zcta <- data.frame()
zcta_list <- c("Zip_ZHVI", "Zip_ZORI")
for (zcta in zcta_list) {
  if (nrow(df_zcta) == 0) {
    df_zcta <- prep_zillow(zcta)
    df_zcta <- df_zcta %>% rename(Date = date)
  }
  else {
    df_zcta <- merge(
      df_zcta,
      prep_zillow(zcta),
      by = c("ID", "State", "Year", "Month", "MetroName"),
      all = TRUE
    )
    df_zcta <- df_zcta %>% select(-date)
  }
}

# Create ZCTA-CBSA crosswalk
cbsa_metros <- cbsa %>%
  select(ID, MetroName) %>%
  distinct() %>%
  rename(CBSAFips = ID)

cw_zcta_cbsa <- fread("xwalks/cw_zcta_cbsa.csv")
cw_zcta_cbsa <- cw_zcta_cbsa %>%
  group_by(zcta5) %>%
  filter(abs(afact - max(afact)) < 0.001) %>%
  select(zcta5, cbsa10, zipname) %>%
  rename(
    ID = zcta5,
    CBSAFips = cbsa10,
    ZipName = zipname
  ) %>%
  right_join(cbsa_metros, by = "CBSAFips")

# Create NHGIS ZCTA
nhgis_zcta <- nhgis_zcta %>%
  inner_join(cw_zcta_cbsa, by = "ID") 

nhgis_zcta  <- nhgis_zcta%>%
  group_by(CBSAFips) %>%
  filter(abs(PopDensity - max(PopDensity, na.rm = T)) < 2) %>%
  select(CentralLat = Latitude, CentralLon = Longitude) %>%
  distinct() %>%
  right_join(nhgis_zcta, by = 'CBSAFips') %>%
  ungroup()
p1 <- matrix(c(nhgis_zcta$Longitude, nhgis_zcta$Latitude), ncol = 2)
p2 <- matrix(c(nhgis_zcta$CentralLon, nhgis_zcta$CentralLat), ncol = 2)
nhgis_zcta$DistCBD <- distGeo(p1, p2) / 10^3
nhgis_zcta$LogDistCBD <- log(1 + nhgis_zcta$DistCBD)
save(nhgis_zcta, file = "../../app/nhgis_zcta.RData")

# Create Zillow ZCTA datasets
zcta <- df_zcta %>% inner_join(cw_zcta_cbsa, by = c('ID', 'MetroName'))
zcta$Date <- as.Date(zcta$Date, "%Y-%m-%d")
zcta <- zcta %>%
  group_by(ID) %>%
  arrange(ID, Date) %>%
  filter(Year >= 2015) %>%
  ungroup()
save(zcta, file = "../../app/zillow_zcta.RData")

# Create ZCTA shapefile/construct ZCTA shape file if it does not exist
# if (!file.exists("../data/shapefiles/zcta_map.shp")) {
zcta_map <- st_read("../shapefiles/zcta")
zcta_map <- zcta_map %>%
  select(starts_with("GEOID"))
colnames(zcta_map) <- c("ID", "geometry")
zcta_map <- zcta_map %>%
  mutate(ID = as.numeric(ID)) %>%
  inner_join(cw_zcta_cbsa, by = "ID")
zcta_map <- zcta_map %>% filter(is.na("ID") == 0)
zcta_map <- st_transform(zcta_map, "+proj=longlat +datum=WGS84")
zcta_map <- ms_simplify(zcta_map, keep = 0.01, keep_shapes = T)
st_write(zcta_map, ".../../app/zcta_map.shp")

# # COVID Data ####
# df_nyt = fread('nytimes/us-counties.csv')
# df_nyt = df_nyt[is.na(fips) == 0,]
# df_nyt = df_nyt[floor(df_nyt$fips / 1000) <= 56,]
# mycbsas = cbsa %>%
#   select(MetroName, ID, Latitude, Longitude, Pop) %>%
#   distinct()
# cw_cbsa = fread('xwalks/cw_counties_cbsa_2010.csv')
# cw_cbsa = merge(cw_cbsa, mycbsas, by.x = 'cbsa10', by.y = 'ID')
# df_nyt = merge(cw_cbsa,
#                df_nyt,
#                by.x = 'county',
#                by.y = 'fips',
#                all = T)
# df_nyt = df_nyt %>%
#   group_by(MetroName, Year = year(date), Month = month(date)) %>%
#   summarize(
#     TotalCases = sum(cases * afact),
#     TotalDeaths = sum(deaths * afact),
#     Pop = sum(Pop * afact)
#   ) %>%
#   mutate(CasesPerCapita = TotalCases / Pop,
#          DeathsPerCapita = TotalDeaths / Pop)
# save(df_nyt, file = '../../app/nyt.RData')
#
# # Protest Data ####
# df_alced = fread('acled/acled.csv')
# colnames(df_alced) = tolower(colnames(df_alced))
# df_alced$event_date = gsub(" ", "-", df_alced$event_date)
# df_alced$Date = as.Date(df_alced$event_date, "%d-%b-%Y")
# df_alced = df_alced %>%
#   filter(country == 'United States') %>%
#   group_by(
#     Year = year(Date),
#     Month = month(Date),
#     Location = location,
#     Latitude = latitude,
#     Longitude = longitude,
#     EventType = event_type
#   ) %>%
#   summarize(n = n())
# save(df_alced, file = '../../app/alced.RData')
