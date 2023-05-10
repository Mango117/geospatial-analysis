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

SESData <- readr::read_csv(
  here::here("ABSData", 
             "SES-Data", 
             "IRSAD.csv"))



## --- TransportData Import and Clean ----
transportData <- readr::read_csv(
  here::here("Data", 
             "transport_data_MOD2.csv"))

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
#Remove row with electric car
cleanData <- cleanData[-c(33), ]


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
#cleanData$Fuel_Type <- replace_na(cleanData$Fuel_Type, "P")
#cleanData$Fuel_Type <-replace(cleanData$Fuel_Type, cleanData$Fuel_Type == "E" , "P")
class(cleanData$Fuel_Type) = "character"


#G for gasoline
f <- function(t){
  ifelse(cleanData$Fuel_Type == "G", ((cleanData$`Fuel _Consumption` * cleanData$MMCDist)/100 * 1.76 + cleanData$Cost_parking),cleanData$Cost)
}
cleanData$Cost <- f(cleanData)

#D for Diesel
f <- function(t){
  ifelse(cleanData$Fuel_Type == "D", ((cleanData$`Fuel _Consumption` * cleanData$MMCDist)/100 * 2.31 + cleanData$Cost_parking),cleanData$Cost)
}
cleanData$Cost <- f(cleanData)

#Hospital Transport
f <- function(t){
  ifelse(cleanData$Transport == "E", (cleanData$Hospital_trans_cost),cleanData$Cost)
}
cleanData$Cost <- f(cleanData)

h <- function(y){
  ifelse(is.na(cleanData$Cost), cleanData$Public_cost, cleanData$Cost)
}
cleanData$Cost <- h(cleanData)

class(cleanData$Cost) = "numeric"




#Add SES Data to cleanData

class(SESData$POA_Code) = "character"

cleanData["Resident_Pop"] <- NA
cleanData["Score"] <- NA
cleanData["AUS_Rank"] <- NA
cleanData["AUS_Decile"] <- NA
cleanData["AUS_Percentile"] <- NA
cleanData["State"] <- NA
cleanData["State_Rank"] <- NA
cleanData["State_Decile"] <- NA
cleanData["State_Percentile"] <- NA


for (i in cleanData$Zip){
  for (j in SESData$POA_Code){
    if (i == j){
      cleanData$Resident_Pop[cleanData$Zip == i] <- SESData$Resident_Pop[SESData$POA_Code == j]
      cleanData$Score[cleanData$Zip == i] <- SESData$Score[SESData$POA_Code == j]
      cleanData$AUS_Rank[cleanData$Zip == i] <- SESData$AUS_Rank[SESData$POA_Code == j]
      cleanData$AUS_Decile[cleanData$Zip == i] <- SESData$AUS_Decile[SESData$POA_Code == j]
      cleanData$AUS_Percentile[cleanData$Zip == i] <- SESData$AUS_Percentile[SESData$POA_Code == j]
      cleanData$State[cleanData$Zip == i] <- SESData$State[SESData$POA_Code == j]
      cleanData$State_Rank[cleanData$Zip == i] <- SESData$State_Rank[SESData$POA_Code == j]
      cleanData$State_Decile[cleanData$Zip == i] <- SESData$State_Decile[SESData$POA_Code == j]
      cleanData$State_Percentile[cleanData$Zip == i] <- SESData$State_Percentile[SESData$POA_Code == j]
    }
  }
}





# ---- Write BoxPlots ----

#Transport Type all
table(cleanData$Transport)
ptv_data <- subset(cleanData, Transport == "A")
taxi_data <- subset(cleanData, Transport == "C")
car_data <- subset(cleanData, Transport == "D")
hosptrans_data <- subset(cleanData, Transport == "E")
boxplot(ptv_data$Cost, taxi_data$Cost, car_data$Cost, hosptrans_data$Cost, main = "Comparison of transport types to MMC", ylab = ("Total Cost ($)"), xlab = "Transport Type", 
        names = c("Public Transport(n=17)", "Taxi(n=4)", "Car(n=133)", "Hospital Transport(n=1)"), col = c("orange","red", "yellow", "green"))


#Travel time all
table(cleanData$Time_trans)
Time_under_20mins <- subset(cleanData, Time_trans == "A")
Time_20mins_1hr <- subset(cleanData, Time_trans == "B")
Time_1hr_2hr <- subset(cleanData, Time_trans == "C")
Time_over_2hrs <- subset(cleanData, Time_trans == "D")
boxplot(Time_under_20mins$Cost, Time_20mins_1hr$Cost, Time_1hr_2hr$Cost, Time_over_2hrs$Cost, main = "Comparison of times to MMC", ylab = ("Total Cost ($)"), xlab = "Travel Time", 
        names = c("<20min (n=38)", "20min-1hr (n=84)", "1hr-2hr(n=29)", ">2hr(n=4)"), col = c("orange","red", "yellow", "green"))



#Transport Type car vs ptv
table(cleanData$Fuel_Type)
ptv_data <- subset(cleanData, Transport == "A")
car_data <- subset(cleanData, Fuel_Type == "G" | Fuel_Type == "D")
boxplot(car_data$Cost, ptv_data$Cost, main = "Comparison of ptv vs car prices to MMC", ylab = ("Total Cost ($)"), xlab = "Transport Type", 
        names = c("Car (132)", "Public Transport (17)"), col = c("orange","red"))


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
boxplot(unassisted$Cost, stick$Cost, walker$Cost, wheelchair$Cost,  main = "Comparison of visitor ambulatory status", ylab = ("Total Cost ($)"), xlab = "Ambulatory Status", 
        names = c("Unassisted(n=135)", "Stick(n=10)", "Walker(n=7)", "Wheelchair(n=3)"), col = c("orange","red", "yellow", "green"))


#Combined plot of transport and ambulatory status vs cost
library(ggplot2)
cleanData$Transport_Class <- ifelse(cleanData$Transport %in% c("D"), "Car",
                                    ifelse(cleanData$Transport %in% c("A"), "Public Transport", "Other"))


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



#combined scatter plot
cleanData$Time_trans <- replace(cleanData$Time_trans, cleanData$Time_trans == "A", "1) <20min(n=38)")
cleanData$Time_trans <- replace(cleanData$Time_trans, cleanData$Time_trans == "B", "2) 20min-1hr(n=84)")
cleanData$Time_trans <- replace(cleanData$Time_trans, cleanData$Time_trans == "C", "3) 1hr-2hr(29)")
cleanData$Time_trans <- replace(cleanData$Time_trans, cleanData$Time_trans == "D", "4) >2hr(4)")
ggplot(cleanData, aes(x = Time_trans, y = Cost, color = MMCDist)) +
  geom_jitter(aes(shape = Ambulatorystatus), width = 0.3, size = 3) + 
  labs(x = "Time taken to travel to MMC", y = "Cost ($)", color = "Distance from MMC (km)") + 
  theme_bw() + 
  scale_shape_manual(values = c(15, 16, 17, 18), name = "Ambulatory Status", labels = c("A=Unassisted", "B=Stick", "C=Walker", "D=Wheelchair")) + 
  scale_color_gradient(low = "#fc0303", high = "#fcfc03")

  


#combined scatter plot
ggplot(cleanData, aes(x = Transport_Class, y = Cost, color = State_Percentile)) + 
  geom_jitter(aes(shape = Ambulatorystatus), width = 0.3, size = 3) + 
  labs(x = "Transport Class", y = "Cost", color = "State SES Percentile") + 
  theme_bw() + 
  scale_shape_manual(values = c(15, 16, 17, 18), name = "Ambulatory Status", labels = c("A=Unassisted", "B=Stick", "C=Walker", "D=Wheelchair")) + 
  scale_color_gradient(low = "#000291", high = "#70cdff")


  

#cost of parking %
drivers <- filter(cleanData, Transport_Class == "Car")
drivers$percentpark <- drivers$Cost_parking / drivers$Cost


drivers <- drivers[complete.cases(drivers$percentpark), ]
mean(drivers$percentpark)
median(drivers$percentpark)
max(drivers$percentpark)



#scatter plot of cost vs MMCDist
ggplot(drivers, aes(x = MMCDist, y = Cost, color = MMCDist)) + 
  geom_point() + 
  scale_color_gradient(low = "#fc0303", high = "#fcfc03") + 
  geom_smooth(method = "lm", se = TRUE) + 
  labs(x = "Distance from MMC (km)", y = "Cost ($)", color = "Distance(km)")


#spearman rho of cost vs MMCDist
result = cor(drivers$Cost, drivers$MMCDist, method = "spearman")
cat("Spearman correlation coefficient is:", result)
count(drivers)
df = count(drivers) - 2
cor.test(drivers$Cost, drivers$MMCDist, method = "spearman")

#linear regression of cost vs MMCDist
model <- lm(Cost ~ MMCDist, data = drivers)
summary(model)


#scatter plot of cost vs SES
ggplot(drivers, aes(x = State_Percentile, y = Cost, color = State_Percentile)) + 
  geom_point() + 
  scale_color_gradient(low = "#000291", high = "#70cdff") + 
  geom_smooth(method = "lm", se = TRUE)


#spearman rho of cost vs SES
result = cor(drivers$Cost, drivers$State_Percentile, method = "spearman")
cat("Spearman correlation coefficient is:", result)
count(drivers)
df = count(drivers) - 2
cor.test(drivers$Cost, drivers$State_Percentile, method = "spearman")


#linear regression of cost vs SES
model <- lm(Cost ~ State_Percentile, data = drivers)
summary(model)


#scatter plot of MMCDist vs SES
ggplot(drivers, aes(x = State_Percentile, y = MMCDist, color = State_Percentile)) + 
  geom_point() + 
  scale_color_gradient(low = "#005c22", high = "#27f231") + 
  geom_smooth(method = "lm", se = TRUE) + 
  labs(x = "IRSAD Percentile relative to VIC", y = "Distance from MMC (km)", color = "Distance(km)")


#spearman rho of MMCDist vs SES
result = cor(drivers$MMCDist, drivers$State_Percentile, method = "spearman")
cat("Spearman correlation coefficient is:", result)
count(drivers)
df = count(drivers) - 2
cor.test(drivers$MMCDist, drivers$State_Percentile, method = "spearman")


#linear regression o MMCDist vs SES
model <- lm(MMCDist ~ State_Percentile, data = drivers)
summary(model)



#Kruskal-Wallis test of time vs cost
continuous_cost <- Time_under_20mins$Cost
categorical <- rep(c("Under20"), each = nrow(Time_under_20mins))
group1data <- data.frame(c(continuous_cost), categorical)

continuous_cost <- Time_20mins_1hr$Cost
categorical <- rep(c("From20to1"), each = nrow(Time_20mins_1hr))
group2data <- data.frame(c(continuous_cost), categorical)

continuous_cost <- Time_1hr_2hr$Cost
categorical <- rep(c("From1to2"), each = nrow(Time_1hr_2hr))
group3data <- data.frame(c(continuous_cost), categorical)

continuous_cost <- Time_over_2hrs$Cost
categorical <- rep(c("Over2"), each = nrow(Time_over_2hrs))
group4data <- data.frame(c(continuous_cost), categorical)

combined_df <- rbind(group1data, group2data, group3data, group4data)

kruskal.test(combined_df[, 1] ~ combined_df[, 2])




#mean prices for car vs ptv
carprice <- filter(cleanData, Transport_Class == "Car")
mean(carprice$Cost)
mean(carprice$State_Percentile)

ptvprice <- filter(cleanData, Transport_Class == "Public Transport")
mean(ptvprice$Cost)
mean(ptvprice$State_Percentile)




# ---- Demographic data table ----
library(finalfit)

explanatory <- c("Vistortype", "Age", "Ambulatorystatus", "Time_trans", "State_Percentile", "MMCDist", "Cost")
demotab <- cleanData %>%
  summary_factorlist("Transport", explanatory,
                     p=TRUE, na_include=TRUE)
write.csv(demotab, "/Users/manojarachige/Downloads/demotab.csv", row.names = TRUE)



# ---- Percentage Parking ----
car_data["PercentPark"] <- car_data$Cost_parking/car_data$Cost
car_data_noNA <- car_data[complete.cases(car_data$PercentPark),]
mean(car_data_noNA$PercentPark)



# ---- Create a Pivot Table and display chloropleth using leaflet ----
library(dplyr)
library(tidyr)

## Create a pivot table
pivot_table <- cleanData %>%
  group_by(Zip) %>%
  summarize(avg_cost = mean(Cost), avg_distance = mean(MMCDist), avg_ses = mean(State_Percentile), count = n())

## right join to the geo data
geoData <- right_join(basicDemographicsVIC, pivot_table, 
                            by=c("POA_CODE16" = "Zip"))

## Reproduce the existing area column as a demo.
geoData <- mutate(geoData, 
                        PostcodeArea=units::set_units(st_area(geometry), km^2))

## Distance to MMC
geoData <- sf::st_transform( geoData, crs = sf::st_crs( MMCLocation ) )
geoData <- mutate(geoData, 
                        DistanceToMMCforVisit=units::set_units(st_distance(geometry,MMCLocation)[,1], km))



## comments re auto-great-circle, straight line assumption, 
## distance to polygon or polygon-centroid ...
plot(geoData["DistanceToMMCforVisit"])


## plot

## tables to paste into latex
tt <- knitr::kable(select(head(geoData), POA_NAME, Tot_P_P, count, DistanceToMMCforVisit, avg_cost, avg_ses), format="latex")
writeLines(tt, "mmcdemograhics")


## ---- InteractiveDisplay ----
library(tmap)
tmap_mode("view")

MMCLocation <- mutate(MMCLocation, ID="Monash Medical Centre")

tm_shape(geoData, name="Data by Postcode") + 
  tm_polygons("count", id="POA_NAME", popup.vars=c("count"), alpha=0.6, interactive = TRUE) +
  tm_shape(MMCLocation) + tm_markers() + 
  tm_basemap("OpenStreetMap")


names(geoData)[names(geoData) == "avg_ses"] <- "IRSAD"
tm_shape(geoData, name="Data by Postcode") + 
  tm_polygons("IRSAD", id="IRSAD", popup.vars=c("IRSAD"), alpha=0.6, palette = get_brewer_pal("Greens", n = 15, contrast = c(0.15, 1)), interactive = TRUE) +
  tm_shape(MMCLocation) + tm_markers()






# ---- Write CSV after dropping geodata ----
df <- cleanData[-c(24)]
write.csv(df,"/Users/manojarachige/Downloads/cleanData.csv", row.names = TRUE)

mean(cleanData$Cost)
median(cleanData$Cost)

# ---- UP TO HERE ----