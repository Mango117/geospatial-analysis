# Plan for Geospatial Analysis Project

## Overarching goal
??

## Steps 
1. Work out distance between postcode (central) to MMC location using OSM
   1. Initial data "As the crow flies"
   2. NOTE: Might need to use Google Maps API/OSRM to create more accurate depiction with street data
2. Work out fuel consumption of each vehicle
   1. Note: Data for car petrol consumption from https://carsales.com.au using Combined Fuel Consumption. This is due to CarSales having the majority of Australian car makes
   2. Note: If model and make not specified in data, the most popular model was used based on Australian sales for that year, or Global sales if Australian was not available
   3. Note: If a year was not specified, the most recent make was used
   4. Note: If there are multiple models for which data is available, the cheapest model was used
   5. Note: Fuel type was selected as either gasoline or diesel. Electric car were ignored and Public Transport responses were used instead
3. Clean up data
   1. Incorrectly answered responses due to survey respndent mistakes were changed (ie. Question states "If you responded yes to 8a..." and respondent answers the question despite responding "no" to 8a)
   2. Remove respondents who list a postcode outside of VIC
4. Calculate pricing
   1. Straight line distance between postcode and MMC was used for cars
   2. Estimated values from Question 8 of the survey were used for PTV pric calculations, rounded to the highest value in the range

   

## Current Plan
1. Use geospatial paper to test in R
2. Test resources in: https://richardbeare.github.io/GeospatialStroke/Choropleth/mmc_surrounds.html
3. Test resources in: https://github.com/richardbeare/GeospatialStroke/
4. Work out what steps are necesssary to complete project
5. Read and understand Knapsack GA implementation from: https://towardsdatascience.com/genetic-algorithm-in-r-the-knapsack-problem-3edc5b07d4a7 

## Current Progress
1. Created chlorpleth map of the number of visitors to MMC from each postcode. Can be viewed https://rpubs.com/Mango117/948342 
2. Cleanded Data
3. Created CSV of price data for each respondent


## Current Problems
1. Unsure what overarching goal of project is and steps to achieve goal


## Meeting notes 2022-10-12
* Google Map: Use date 6mo in future to avoid using current dynamic data
* Google Map: Shortest route based on time to arrive at destination
* Evaluate difference between Google Maps vs OSRM - Time vs distance? What is the cause of discrepancy
  * Ensure same route between comparisons