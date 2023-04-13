## 2016 census data
# https://datapacks.censusdata.abs.gov.au/datapacks/
# Dempgraphic data by postcode is available, with age information in 
# "ABSData/2016 Census GCP Postal Areas for VIC/2016Census_G01_VIC_POA.csv"

## Boundary data available from the same site

## ---- RPackageCheck ----
ip <- installed.packages () [, 1] # names of installed packages
requiredpackages <- c("tidyverse", "sf", "here", 
                      "units",  "tmaptools", "tmap", "knitr", "GA")
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
library(GA)


## ---- SetWD ----
setwd("/Users/manojarachige/Documents/Coding/Projects/Geospatial Analysis/geospatial-analysis/")


## ---- CensusData ----
postcodeboundariesAUS <- sf::read_sf(
  here::here("ABSData", 
            "Boundaries", 
            "POA_2016_AUST.shp"))

basicDemographicsVIC <- readr::read_csv(
  here::here("ABSData", 
            "2016 Census GCP Postal Areas for VIC", 
            "2016Census_G01_VIC_POA.csv"))



## --- TransportData Import and Clean ----
transportData <- readr::read_csv(
  here::here("Data", 
             "transport_data_MOD.csv"))

colnames(transportData) <- c(transportData[1,])
transportData <-transportData[-c(1),]
cleanData <-transportData[c(1:7,9:10,14:27)]
class(cleanData$Zip) = "character"
class(cleanData$`Fuel _Consumption`) = "numeric"
cleanData$`Fuel _Consumption`[is.na(cleanData$`Fuel _Consumption`)] <- 0





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




## ---- CalculateCost ----
#Add Area of geo locations to basicDemographicsVIC
basicDemographicsVIC <- mutate(basicDemographicsVIC, 
                    PostcodeArea=units::set_units(st_area(geometry), km^2))
basicDemographicsVIC <- sf::st_transform( basicDemographicsVIC, crs = sf::st_crs( MMCLocation ) )
basicDemographicsVIC <- mutate(basicDemographicsVIC, 
                    DistanceToMMCforVisit=units::set_units(st_distance(geometry,MMCLocation)[,1], km))


#Add geo data and area of locations to cleanData
cleanData['geodata'] <- NA
cleanData['ZipArea'] <- NA
cleanData['MMCDist'] <- NA
cleanData['Cost'] <- NA

for (i in cleanData$Zip){
  for (j in basicDemographicsVIC$POA_CODE16){
    if (i == j){
      cleanData$geodata[cleanData$Zip == i] <- basicDemographicsVIC$geometry[basicDemographicsVIC$POA_CODE16 == j]
      cleanData$ZipArea[cleanData$Zip == i] <- basicDemographicsVIC$PostcodeArea[basicDemographicsVIC$POA_CODE16 == j]
      cleanData$MMCDist[cleanData$Zip == i] <- basicDemographicsVIC$DistanceToMMCforVisit[basicDemographicsVIC$POA_CODE16 == j]
    }
  }
}



#Remove row with invalid postcode
cleanData <- cleanData[-c(73), ]


#parking prices
cleanData$Cost_parking[cleanData$Cost_parking == "A"] <- 11
cleanData$Cost_parking[cleanData$Cost_parking == "B"] <- 15
cleanData$Cost_parking[cleanData$Cost_parking == "C"] <- 17
cleanData$Cost_parking[cleanData$Cost_parking == "D"] <- 19
cleanData$Cost_parking[cleanData$Cost_parking == "E"] <- 22
cleanData$Cost_parking[cleanData$Cost_parking == "F"] <- 24
cleanData$Cost_parking[cleanData$Cost_parking == "G"] <- 25
cleanData$Cost_parking[cleanData$Cost_parking == "H"] <- 30
cleanData$Cost_parking[cleanData$Cost_parking == "I"] <- 64
cleanData$Cost_parking[cleanData$Cost_parking == "J"] <- 0
cleanData$Cost_parking[cleanData$Cost_parking == "K"] <- 0
class(cleanData$Cost_parking) = "double"


#ptv prices
cleanData$Public_cost[cleanData$Public_cost == "A"] <- 9
cleanData$Public_cost[cleanData$Public_cost == "B"] <- 19
cleanData$Public_cost[cleanData$Public_cost == "C"] <- 29
cleanData$Public_cost[cleanData$Public_cost == "D"] <- 39
cleanData$Public_cost[cleanData$Public_cost == "E"] <- 49
cleanData$Public_cost[cleanData$Public_cost == "F"] <- 59
cleanData$Public_cost[cleanData$Public_cost == "G"] <- 0
class(cleanData$Public_cost) = "double"


#fuel prices from https://fuelprice.io/vic/melbourne/unleaded/ 

#replace NA with P and E with P
cleanData$Fuel_Type <- replace_na(cleanData$Fuel_Type, "P")
cleanData$Fuel_Type <-replace(cleanData$Fuel_Type, cleanData$Fuel_Type == "E" , "P")
class(cleanData$Fuel_Type) = "character"



f <- function(t){
  ifelse(cleanData$Fuel_Type == "G", ((cleanData$`Fuel _Consumption` * cleanData$MMCDist)/100 * 1.76 + cleanData$Cost_parking),cleanData$Cost)
}
cleanData$Cost <- f(cleanData)

f <- function(t){
  ifelse(cleanData$Fuel_Type == "D", ((cleanData$`Fuel _Consumption` * cleanData$MMCDist)/100 * 2.31 + cleanData$Cost_parking),cleanData$Cost)
}
cleanData$Cost <- f(cleanData)

h <- function(y){
  ifelse(is.na(cleanData$Cost), cleanData$Public_cost, cleanData$Cost)
}
cleanData$Cost <- h(cleanData)


# ---- Write BoxPlots ----
#Transport Type
table(cleanData$Fuel_Type)
car_data <- subset(cleanData, Fuel_Type == "G" | Fuel_Type == "D")
ptv_data <- subset(cleanData, Fuel_Type == "P")
boxplot(car_data$Cost, ptv_data$Cost, main = "Comparison of ptv vs car prices to MMC", ylab = ("Total Cost ($)"), xlab = "Transport Type", 
        names = c("Car (132)", "Public Transport (25)"), col = c("orange","red"))


#Visitor Type
table(cleanData$Vistortype)
inpt_data <- subset(cleanData, Vistortype == "A")
outpt_data <- subset(cleanData, Vistortype == "B")
visit_data <- subset(cleanData, Vistortype == "C")
boxplot(inpt_data$Cost, outpt_data$Cost, visit_data$Cost, main = "Comparison of visitor types", ylab = ("Total Cost ($)"), xlab = "Visitor Type", 
        names = c("Inpatient(3)", "Outpatient(102)", "Visitor(52)"), col = c("orange","red", "yellow"))


#Visitor Type
table(cleanData$Age)
age_to_29 <- subset(cleanData, Age == "A")
age_to_39 <- subset(cleanData, Age == "B")
age_to_59 <- subset(cleanData, Age == "C")
age_to_79 <- subset(cleanData, Age == "D")
age_over_80 <- subset(cleanData, Age == "E")
boxplot(age_to_29$Cost, age_to_39$Cost, age_to_59$Cost, age_to_79$Cost, age_over_80$Cost,  main = "Comparison of visitor ages", ylab = ("Total Cost ($)"), xlab = "Visitor Age", 
        names = c("18-29(14)", "30-39(12)", "40-59(42)", "60-79(77)", "Over 80(12)"), col = c("orange","red", "yellow", "green", "blue"))


#Ambulatory Type
table(cleanData$Ambulatorystatus)
unassisted <- subset(cleanData, Ambulatorystatus == "A")
stick <- subset(cleanData, Ambulatorystatus == "B")
walker <- subset(cleanData, Ambulatorystatus == "C")
wheelchair <- subset(cleanData, Ambulatorystatus == "D")
boxplot(unassisted$Cost, stick$Cost, walker$Cost, wheelchair$Cost,  main = "Comparison of visitor ambulatory status", ylab = ("Total Cost ($)"), xlab = "Visitor Age", 
        names = c("Unassisted(137)", "Stick(10)", "Walker(7)", "Wheelchair(3)"), col = c("orange","red", "yellow", "green"))


#Combined plot of transport and ambulatory status vs cost
library(ggplot2)

cleanData$Transport_Class <- ifelse(cleanData$Fuel_Type %in% c("G", "D"), "Car",
                                    ifelse(cleanData$Fuel_Type %in% c("P"), "Public Transport", "Other"))

#box plot
ggplot(cleanData, aes(x = interaction(Transport_Class, Ambulatorystatus), y = Cost, fill = Ambulatorystatus)) + 
  geom_boxplot() + 
  labs(x = "Transport and Ambulatory Status", y = "Cost") + 
  theme_bw() + 
  scale_fill_discrete(name = cleanData$Ambulatorystatus, labels = c("A=Unassisted", "B=Stick", "C=Walker", "D=Wheelchair"))

#box plot
ggplot(cleanData, aes(x = interaction(Transport_Class, Ambulatorystatus), y = Cost, fill = Ambulatorystatus)) + 
  geom_boxplot() + 
  labs(x = "Transport and Ambulatory Status", y = "Cost", color = "Ambulatory Status") + 
  theme_bw() + 
  geom_jitter(
    mapping = NULL,
    data = NULL,
    stat = "identity",
    position = "jitter",
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE
  ) +
  scale_fill_discrete(name = cleanData$Ambulatorystatus, labels = c("A=Unassisted", "B=Stick", "C=Walker", "D=Wheelchair"))


#scatter plot
ggplot(cleanData, aes(x = interaction(Transport_Class, Ambulatorystatus), y = Cost, color = Ambulatorystatus)) + 
  geom_point() +
  labs(x = "Transport and Ambulatory Status", y = "Cost", color = "Ambulatory Status") +
  theme_bw() + 
  scale_color_discrete(name = "Ambulatory Status", labels = c("A=Unassisted", "B=Stick", "C=Walker", "D=Wheelchair"))

#scatter plot
ggplot(cleanData, aes(x = interaction(Transport_Class, Ambulatorystatus), y = Cost, color = Ambulatorystatus)) + 
  geom_point() +
  labs(x = "Transport and Ambulatory Status", y = "Cost", color = "Ambulatory Status") +
  theme_bw() + 
  geom_jitter(
    mapping = NULL,
    data = NULL,
    stat = "identity",
    position = "jitter",
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE
  ) +
  scale_color_discrete(name = "Ambulatory Status", labels = c("A=Unassisted", "B=Stick", "C=Walker", "D=Wheelchair"))
  
#combined scatter plot

ggplot(cleanData, aes(x = Transport_Class, y = Cost, color = MMCDist)) + 
  geom_jitter(aes(shape = Ambulatorystatus), width = 0.3, size = 3) + 
  labs(x = "Transport Class", y = "Cost", color = "Distance from MMC") + 
  theme_bw() + 
  scale_shape_manual(values = c(15, 16, 17, 18), name = "Ambulatory Status", labels = c("A=Unassisted", "B=Stick", "C=Walker", "D=Wheelchair")) + 
  scale_color_gradient(low = "#fc0303", high = "#fcfc03")
  
  


#cost of parking %
drivers <- filter(cleanData, Transport_Class == "Car")
drivers$percentpark <- drivers$Cost_parking / drivers$Cost


drivers <- drivers[complete.cases(drivers$percentpark), ]
mean(drivers$percentpark)
median(drivers$percentpark)
max(drivers$percentpark)



#scatter plot of cost vs car
ggplot(drivers, aes(x = MMCDist, y = Cost, color = MMCDist)) + 
  geom_point() + 
  scale_color_gradient(low = "#fc0303", high = "#fcfc03") + 
  geom_smooth(method = "lm", se = TRUE)


#spearman rho of cost vs car
result = cor(drivers$Cost, drivers$MMCDist, method = "spearman")
cat("Spearman correlation coefficient is:", result)
count(drivers)
df = count(drivers) - 2
cor.test(drivers$Cost, drivers$MMCDist, method = "spearman")


# ---- Write CSV after dropping geodata ----
df <- cleanData[-c(24)]
write.csv(df,"/Users/manojarachige/Downloads/cleanData.csv", row.names = TRUE)

mean(cleanData$Cost)
median(cleanData$Cost)

# ---- UP TO HERE ----