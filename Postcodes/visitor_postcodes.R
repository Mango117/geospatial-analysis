## 2016 census data
# https://datapacks.censusdata.abs.gov.au/datapacks/
# Dempgraphic data by postcode is available, with age information in 
# "ABSData/2016 Census GCP Postal Areas for VIC/2016Census_G01_VIC_POA.csv"

## Boundary data available from the same site

## ---- RPackageCheck ----
ip <- installed.packages () [, 1] # names of installed packages
requiredpackages <- c("tidyverse", "sf", "here", 
                      "units",  "tmaptools", "tmap", "knitr")
if (!all(requiredpackages %in% ip)) {
  msg <- paste("This script requires the following packages: ", paste(requiredpackages, collapse=", "))
  message(msg)
  message("Attempting to install them")
  options(repos=c(CRAN="https://cloud.r-project.org"))
  missingCRAN <- setdiff(requiredpackages, ip)
  if (length(missingCRAN) > 0) {
    message(paste("Missing packages are", missingCRAN))
    install.packages(missingCRAN)
  }
}
## ---- Libraries ----
library(tidyverse)
library(sf)
library(units)
library(tmaptools)

## ---- CensusData ----
postcodeboundariesAUS <- sf::read_sf(
  here::here("ABSData", 
            "Boundaries", 
            "POA_2016_AUST.shp"))

basicDemographicsVIC <- readr::read_csv(
  here::here("ABSData", 
            "2016 Census GCP Postal Areas for VIC", 
            "2016Census_G01_VIC_POA.csv"))

## ---- MonashMedicalCentre ----
## Location of hopsital providing acute stroke services
## address: 246 Clayton Rd, Clayton VIC, 3168
MMCLocation <- tmaptools::geocode_OSM("Monash Medical Centre, Clayton, Victoria, Australia", as.sf=TRUE)
MMCLocation

## ---- JoinCensusAndBoundaries ----
## Join the demographics and shape tables, retaining victoria only
## use postcode boundaries as the reference data frame so that coordinate
## reference system is retained.
basicDemographicsVIC <- right_join(postcodeboundariesAUS, basicDemographicsVIC, 
                                  by=c("POA_CODE" = "POA_CODE_2016"))

## ---- ImportTransportData ----
#import data from transport_data.csv
transportData <- readr::read_csv(
  here::here("Data",
             "transport_data.csv"))
#Convert N/A values to 0
transportData[is.na(transportData)] <- 0
class(transportData$ZIP) = "character"

## ---- Join transportData and basic DemographicsVIC ----
transportData <- right_join(basicDemographicsVIC, transportData, 
                                   by=c("POA_CODE16" = "ZIP"))


## ---- SpatialComputations ----
## Add some geospatial measures to the data frame
## Reproduce the existing area column as a demo.
transportData <- mutate(transportData, 
                               PostcodeArea=units::set_units(st_area(geometry), km^2))

## Distance to MMC
transportData <- sf::st_transform( transportData, crs = sf::st_crs( MMCLocation ) )
transportData <- mutate(transportData, 
                               DistanceToMMCforVisit=units::set_units(st_distance(geometry,MMCLocation)[,1], km))



## comments re auto-great-circle, straight line assumption, 
## distance to polygon or polygon-centroid ...
plot(transportData["DistanceToMMCforVisit"])





## ---- FilteringPostcodes ----
## Make a small dataset for MMC surrounds.
basicTransportData <- filter(transportData, DistanceToMMC < set_units(20, km))

## ---- PostcodesTable ----
## tables to paste into latex
tt <- knitr::kable(select(head(basicTransportData), POA_NAME, Tot_P_P, number_patients, DistanceToMMC), format="latex")
writeLines(tt, "mmcdemograhics")
## ---- InteractiveDisplay ----
library(tmap)
tmap_mode("view")

MMCLocation <- mutate(MMCLocation, ID="Monash Medical Centre")

tm_shape(basicTransportData, name="Number of Visitors") + 
  tm_polygons("number_patients", id="POA_NAME", popup.vars=c("Patients"="number_patients"), alpha=0.6) + 
  tm_shape(MMCLocation) + tm_markers() + 
  tm_basemap("OpenStreetMap")








