#################
# Using cartograms to jointly depict exposure/outcome data
# Citation: Tran NK, Goldstein ND. Jointly Representing Geographic Exposure and Outcome Data Using Cartograms. Manuscript in preparation.
# Requires:
#   1) Census API key for tidycensus: http://api.census.gov/data/key_signup.html
#   2) Census tract shapefile for Delaware, 2010: https://www.census.gov/geo/maps-data/data/tiger-line.html
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

census_api_key("paste census API key here")
income = get_acs(geography="tract", variable="B19013_001", year=2016, survey="acs5", state="Delaware")
fertility = get_acs(geography="tract", variable="B13002_002", year=2016, survey="acs5", state="Delaware")
population = get_acs(geography="tract", variable="B01003_001", year=2016, survey="acs5", state="Delaware")


### ADD CENSUS TRACT DATA to SHAPEFILE ###

setwd("specify path to shapefiles")

#read DE census tracts
de_ct = readShapePoly("tl_2010_10_tract10/tl_2010_10_tract10")

#tally data by census tract
data_by_tract = data.frame("NAME10"=de_ct@data$NAME10, stringsAsFactors=F)
data_by_tract$fertility = NA
data_by_tract$income = NA
data_by_tract$population = NA
for (i in 1:nrow(data_by_tract))
{
  data_by_tract$fertility[i] = fertility$estimate[grep(paste("Census Tract ", as.character(data_by_tract$NAME10[i]), ",", sep=""), fertility$NAME)][1]
  data_by_tract$income[i] = income$estimate[grep(paste("Census Tract ", as.character(data_by_tract$NAME10[i]), ",", sep=""), income$NAME)][1]
  data_by_tract$population[i] = population$estimate[grep(paste("Census Tract ", as.character(data_by_tract$NAME10[i]), ",", sep=""), population$NAME)][1]
}
rm(i)

#per-capita
data_by_tract$fertility_percapita = data_by_tract$fertility/data_by_tract$population*1000

#check for expected association
summary(lm(data_by_tract$fertility_percapita ~ scale(data_by_tract$income)))
confint(lm(data_by_tract$fertility_percapita ~ scale(data_by_tract$income)))

#merge cases by census tract with census tract data; again modify NAME variable if needed
#generates an ignorable warning due to duplicate geometry in shapefile
de_ct = merge(x=de_ct, y=data_by_tract, by="NAME10", all.x=T, duplicateGeoms=T)

# cholorpleth of median income and fertility by census tract 
spplot(de_ct, "income", cuts=3, col.regions=c("#FFFFFF", "#D9D9D9", "#969696", "#252525"), main = "Supplementary Figure 1. Delaware Median Income by Census Tract")
spplot(de_ct, "fertility_percapita", cuts=3, col.regions=c("#FFFFFF", "#D9D9D9", "#969696", "#252525"), main = "Supplementary Figure 2. Delaware Per-Capita Fertility by Census Tract")

# 1 variable cartogram for median income - itermax= indicates the amount of distortion of shapes
de_carto = cartogram_cont(de_ct, "income", itermax=4)
plot(de_carto, main="distorted (sp)") # distorted map based on income
plot(de_ct, main="normal") # non-distorted map

# 2 variable continuous cartogram for median income and fertility_percapita by census tract
spplot(de_carto, "fertility_percapita", cuts=3, col.regions=c("#FFFFFF", "#D9D9D9", "#969696", "#252525"))
spplot(de_carto, "fertility_percapita", cuts=3, col.regions=c("#FFFFFF", "#D9D9D9", "#969696", "#252525"), main = "Delaware Median Income and Per-Capita Fertility by Census Tracts")

# non-continuous cartogram 
de_cart2 = cartogram_ncont(de_ct, "income", inplace = TRUE)
spplot(de_cart2, "fertility_percapita", cuts=3, col.regions=c("#FFFFFF", "#D9D9D9", "#969696", "#252525"))

# # 1 variable cartogram for fertility_percapita - itermax= indicates the amount of distortion of shapes
# de_carto3 = cartogram_cont(st_as_sf(de_ct), "fertility_percapita", itermax=3)
# plot(as(de_carto3, "Spatial"), main="distorted (sp)") # distorted map based on fertility_percapita
# plot(de_ct, main="normal") # non-distorted map
# 
# # 2 variable continuous cartogram for median income and fertility_percapita by census tract
# spplot(as(de_carto3, "Spatial"), "income", cuts=3, col.regions=c("#FFFFFF", "#D9D9D9", "#969696", "#252525"))
# 
# # non-continuous cartogram 
# de_cart4 = cartogram_ncont(de_ct, "fertility_percapita", inplace = TRUE)
# spplot(de_cart4, "income", cuts=3, col.regions=c("#FFFFFF", "#D9D9D9", "#969696", "#252525"))
