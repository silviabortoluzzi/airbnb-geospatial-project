##############################################
### 1. Reading and manipulating Shapefiles ### 
##############################################

#You can import and store the main geospatial file formats (such as shapefiles 
#and geojson files) in R with the function st_read, contained in the sf package, 
#which creates an object of class sf that can be used to work within the spdep package. 

#To import the shapefile Italy (consisting of Italy.shp, Italy.dbf and Italy.shx), 
#which is a geo-referenced economic dataset for the Italian regions (NUTS 2), type

setwd("...")
library(sf)
italy <- st_read("Italy") #The output is essentially a data frame with geometry information 
                          #attached to it

#Visualizing the spatial content of a shapefile:
plot(st_geometry(italy))
plot(st_centroid(st_geometry(italy)), add = TRUE)

#Visualizing the attributes content of a shapefile
dim(italy)
names(italy)
str(italy)
head(italy)
tail(italy)

#"Geo"      Macroregion
#"gprb"	Log growth rate (1980-2003) of production per capita
#"lninv1b"	PHYSICAL CAPITAL INVESTMENTS (log scale)
#"pr80b"	production per capita 1980
#"pr103b"	production per capita 2003
#"lndens_emp"	EMPLOYMENT DENSITY (log scale)
#"lndens_pop"	Population DENSITY (log scale)
#"lnagrib"	AGRICULTURE EMPLOYMENT SHARE (log scale)

#Selecting subsets of datasets
#Selecting variables of a dataset according to their names
italy$Name
italy$gprb
italy$Geo

#Selecting according to conditions
#selecting regions belonging to S 
subset <- italy[italy$Geo=="S",] 
head(subset)
plot(st_geometry(subset))

#selecting regions not belonging to S 
subset <- italy[italy$Geo!="S",] 
head(subset)
plot(st_geometry(subset))

#selecting regions belonging to NO and having gprb > 0
subset <- italy[italy$Geo=="NO" & italy$gprb > 0,] 
head(subset)
plot(st_geometry(subset))

#selecting regions belonging to NO or NE
subset <- italy[italy$Geo=="NO" | italy$Geo=="NE",] 
head(subset)
plot(st_geometry(subset))

#Adding variables to a shapefile
#you can do that by merging the attributes datasets of the shapefile with 
#the dataset containing the additional variables
#to merge the shapefile of Italian provinces with dataset "employment_prov", type:
prov <- st_read("prov2011_g")
EmplProv <- read.csv("employment_prov.csv")
prov <- merge(prov, EmplProv, by="COD_PRO")
head(prov)

df <- read.csv("../datasets/trentino_listings_maps.csv")


