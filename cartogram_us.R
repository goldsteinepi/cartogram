#################
# Using cartograms to jointly depict exposure/outcome data
# Requires:
#   1) Census API key for tidycensus: http://api.census.gov/data/key_signup.html
#   2) State shapefile for United States, 2017: https://www.census.gov/geo/maps-data/data/cbf/cbf_state.html
#   3) Kaiser Family Foundation State Health Facts for obesity: https://www.kff.org/other/state-indicator/adult-overweightobesity-rate/
#         note: keep only location and rate columns, renaming to "State" and "Rate", delete all other data from csv
#
# 11/1/18 -- Neal Goldstein and Nguyen Tran
#################


### FUNCTIONS ###

library(maptools)     #working with shapefiles
library(tidycensus)   #obtained ACS data
library(maps)         #basic maps for plotting
library(RColorBrewer) #color palette for choropleth maps
library(cartogram)    #mapping cartograms 
library(sf)           #sf (simple feature) object


### READ DATA ###

# working directory
setwd("/data/directory")

# census data
census_api_key("paste census API key here")
income = get_acs(geography="state", variable="B19013_001", year=2016, survey="acs5")
#population = get_acs(geography="state", variable="B01003_001", year=2016, survey="acs5")

# health data
obesity = read.csv(file="obesity.csv", as.is=T)


### ADD HEALTH INDICATOR DATA to SHAPEFILE ###

# read US states
us_state = readShapePoly("cb_2017_us_state_20m/cb_2017_us_state_20m")
contin_48 = subset(us_state, !(NAME %in% c("Alaska", "Hawaii", "Puerto Rico", "District of Columbia")))

# create health indicator dataframe
data_by_state = data.frame("State"=contin_48@data$NAME, stringsAsFactors=F)
data_by_state = merge(x=data_by_state, y=obesity, by="State")
data_by_state = merge(x=data_by_state, y=income, by.x="State", by.y="NAME")
data_by_state$obesity = data_by_state$Rate #renaming variable for convenience
data_by_state$income = data_by_state$estimate #renaming variable for convenience

#merge health indicators with shapefile
contin_48 = merge(x=contin_48, y=data_by_state, by="GEOID", all.x=T, duplicateGeoms=T)

# cholorpleth of median income and obesity by state 
spplot(contin_48, "income", cuts=4, col.regions=c("#FFFFFF", "#F0F0F0", "#D9D9D9", "#969696", "#252525"), main = "United States Median Income")
spplot(contin_48, "obesity", cuts=4, col.regions=c("#FFFFFF", "#F0F0F0", "#D9D9D9", "#969696", "#252525"), main = "United States Overweight and Obesity Rate")

# 1 variable cartogram for median income - itermax= indicates the amount of distortion of shapes
us_carto = cartogram_cont(contin_48, "income", itermax=4)
plot(us_carto, main="distorted (sp)") # distorted map based on income
plot(contin_48, main="normal") # non-distorted map

# 2 variable continuous cartogram for median income and obesity by state
spplot(us_carto, "obesity", cuts=4, col.regions=c("#FFFFFF", "#F0F0F0", "#D9D9D9", "#969696", "#252525"))
spplot(us_carto, "obesity", cuts=4, col.regions=c("#FFFFFF", "#F0F0F0", "#D9D9D9", "#969696", "#252525"), main = "United States Median Income and Obesity Rate")

# non-continuous cartogram 
us_carto2 = cartogram_ncont(contin_48, "income", inplace = TRUE)
spplot(us_carto2, "obesity", cuts=4, col.regions=c("#FFFFFF", "#F0F0F0", "#D9D9D9", "#969696", "#252525"))

# 1 variable cartogram for obesity - itermax= indicates the amount of distortion of shapes
us_carto3 = cartogram_cont(st_as_sf(contin_48), "obesity", itermax=4)
plot(as(us_carto3, "Spatial"), main="distorted (sp)") # distorted map based on obesity
plot(contin_48, main="normal") # non-distorted map

# 2 variable continuous cartogram for median income and obesity by state
spplot(as(us_carto3, "Spatial"), "income", cuts=4, col.regions=c("#FFFFFF", "#F0F0F0", "#D9D9D9", "#969696", "#252525"))

# non-continuous cartogram 
us_carto4 = cartogram_ncont(contin_48, "obesity", inplace = TRUE)
spplot(us_carto4, "income", cuts=4, col.regions=c("#FFFFFF", "#F0F0F0", "#D9D9D9", "#969696", "#252525"))


### REGRESSION CONFIRMATION ###

summary(lm(data_by_state$obesity ~ scale(data_by_state$income)))
confint(lm(data_by_state$obesity ~ scale(data_by_state$income)))
