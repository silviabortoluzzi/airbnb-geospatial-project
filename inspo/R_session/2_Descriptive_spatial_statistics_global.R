
##########################################################################
### 2. Descriptive spatial statistics for areal data (global analysis) ###
##########################################################################

#The package we can use for the analysis of spatial dependence is "spdep"
library(spdep); library(sf); library(tmap)

#Throughout this lab we will use the data about the regional growth in EU2,
setwd("...")
EU <- st_read("EU2")

##########################################
### 1. Creating spatial weights matrices  

#The first step in the creation of a spatial weight matrix is to specify 
#the spatial topology of the data generating process. 
#In other words, we need to define the neighbourhood relationships amongst the spatial units. 

#########
### 1.1 Defining spatial neighbours

#First of all, we need to assign to each spatial unit a unique reference spatial coordinate, 
#say a representative location. The most common choice is to compute the centroid. 
#In R the centroids can be easily computed with function "st_centroid".
coords <- st_centroid(st_geometry(EU))

#To visualize the computed centroids, type
plot(st_geometry(EU), border="blue") 
plot(coords, add = TRUE)

#Note that centroids are arbitrary and, in certain circumstances, 
#may not even fall within the defining polygon. 
#By referring to the distances amongst centroids we can then define 
#the neighbourhood relationships amongst the spatial units. 
#Various definitions of neighbourhood are possible. 
#Here we will apply the k-nearest neighbours, the critical cut-off neighbourhood, 
#and the contiguity-based neighbourhood.

### 1.1.1 k-Nearest neighbours
#The k-nearest neighbours criterion implies that two spatial units are considered as neighbours 
#if their distance is equal, or less than equal, to the minimum possible distance 
#that can be found amongst all the observations. 
#This definition of neighbourhood ensures that each spatial unit has exactly the same number k of neighbours.
#With k = 1, type:
knn1IT <- knn2nb(knearneigh(coords,k=1))
plot(st_geometry(EU), border="grey") 
plot(knn1IT, coords, add=TRUE)

#With k = 4, type:
knn4IT <- knn2nb(knearneigh(coords,k=4))
plot(st_geometry(EU), border="grey") 
plot(knn4IT, coords, add=TRUE) 

### 1.1.2 Critical cut-off neighbourhood
#Critical cut-off neighbourhood criterion implies that two spatial units are considered 
#as neighbours if their distance is equal, or less than equal, to a certain fixed distance 
#which represents a critical cut-off. 
#This threshold distance should not be smaller than a minimum value that ensures 
#that each spatial unit has at least one neighbour.

#Therefore, we first have to compute the minimum threshold distance which allows 
#all regions to have at least one neighbour. We can do that by using the list 
#of k-nearest neighbours with k = 1 and computing the maximum distance 
#separating all pairs of spatial units.
knn1IT <- knn2nb(knearneigh(coords,k=1))
all.linkedT <- max(unlist(nbdists(knn1IT, coords))) 
all.linkedT

#and we found that the minimum threshold distance is equal to 320 km. 
#Therefore, the cut-off distance has to be greater than 320. 
#We can try different neighbourhood definitions for different values of the cut-off distance. 
dnb320 <- dnearneigh(coords, 0, 320); dnb320
dnb420 <- dnearneigh(coords, 0, 420); dnb420 
dnb520 <- dnearneigh(coords, 0, 520); dnb520 
dnb620 <- dnearneigh(coords, 0, 620); dnb620 
dnb720 <- dnearneigh(coords, 0, 720); dnb720 
dnb820 <- dnearneigh(coords, 0, 820); dnb820 

#As the cut-off distance increases, the number of links grows rapidly. We can also see that visually.
plot(st_geometry(EU), border="grey") 
title(main="d nearest neighbours, d = 320-820") 
plot(dnb320, coords, add=TRUE, col="blue")
plot(dnb420, coords, add=TRUE, col="red")
plot(dnb520, coords, add=TRUE, col="yellow")
plot(dnb620, coords, add=TRUE, col="green")
plot(dnb820, coords, add=TRUE, col="grey")

### 1.1.3 Contiguity-based neighbourhood
# Contiguity-based neighbourhood criterion implies that two spatial units are 
# considered as neighbours if they share a common boundary.
contnb_q <- poly2nb(EU, queen=T)
contnb_q
plot(st_geometry(EU), border="grey") 
plot(contnb_q, coords, add=TRUE)
#########
#########

#########
### 1.2 Defining spatial weights

#Once the neighbourhood relationships amongst the observations have been defined, 
#we can create the spatial weights matrix. 

#To create a row-standardized spatial weights matrix for each critical cut-off neighbours 
#list previously created, type
#previously created, type
dnb379.listw <- nb2listw(dnb320,style="W")
dnb420.listw <- nb2listw(dnb420,style="W")
dnb520.listw <- nb2listw(dnb520,style="W")
dnb620.listw <- nb2listw(dnb620,style="W")
dnb720.listw <- nb2listw(dnb720,style="W")
dnb820.listw <- nb2listw(dnb820,style="W")
#########
#########

#########
### 1.3 Building free-form spatial weight matrices 

#For example, build weights as inverse functions of the distance among centroids
distM <- st_distance(coords)/1000
class(distM) <- "matrix" #distance matrix

# Three possible weight matrices
W1 <- 1/(1+distM); diag(W1) <- 0
W2 <- 1/(1+distM^2); diag(W2) <- 0
W3 <- exp(0.1*-distM);diag(W3) <- 0

#However, it is always convenient to avoid matrices that are too dense
#Highly dense Ws lead to underestimation of spatial autocorrelation
#We may set a minimum threshold, for example by setting 0 when the distance
#is greater than 800 km
W1[distM>800] <- 0
W2[distM>800] <- 0
W3[distM>800] <- 0

#Row-standardize them 
W1s <- W1/rowSums(W1) 
W2s <- W2/rowSums(W2) 
W3s <- W3/rowSums(W3) 

#We can convert the weight matrix into a "listw" object (just for computational reasons)
listW1s <- mat2listw(W1s, style="W")
listW2s <- mat2listw(W2s, style="W")
listW3s <- mat2listw(W3s, style="W")
#########
#########

##########################################
##########################################

##########################################
### 2. The Moran's I test of spatial autocorrelation 

#We can see how to perform the global Moran's I test of spatial autocorrelation 
#by referring to the variable EU$gprb, which is the growth rate (1980-2003) 
#of production per capita observed for the Italian regions. 
#The visual inspection of the spatial quantile distribution of the growth rate may suggest 
#the presence of some form of spatial dependence. 

quartiles <- quantile(EU$gprb)
plot(EU[c("gprb")], main="Growth rate 1980-2003", breaks=quartiles) #Using plot.sf

tm_shape(EU) + 
  tm_polygons("gprb", fill.scale = tm_scale_intervals(style="quantile", n=4),
              fill.legend = tm_legend(title="Growth rate 1980-2003")) 
              #using tmap library, see https://tmap.geocompx.org/

#To generate interactive maps that can be rendered on HTML pages
tmap_mode("view") +
tm_basemap(c(StreetMap = "OpenStreetMap", TopoMap = "OpenTopoMap")) +
tm_shape(EU) + 
  tm_polygons("gprb", fill.scale = tm_scale_intervals(style="quantile", n=4),
              fill.legend = tm_legend(title="Growth rate 1980-2003")) 

#The command that allows to perform the global Moran s I test is the function moran.test(). 
#To apply the test to the growth rate with different specifications 
#of the spatial weights matrix (under the assumption of normality), type
moran.test(EU$gprb, dnb379.listw, randomisation=FALSE)
moran.test(EU$gprb, dnb420.listw, randomisation=FALSE)
moran.test(EU$gprb, dnb520.listw, randomisation=FALSE)
moran.test(EU$gprb, dnb620.listw, randomisation=FALSE)
moran.test(EU$gprb, dnb720.listw, randomisation=FALSE) 

#To apply the test to the growth rate with different specifications 
#of the spatial weights matrix (under the assumption of randomisation), type
moran.test(EU$gprb, dnb379.listw, randomisation=TRUE)
moran.test(EU$gprb, dnb420.listw, randomisation=TRUE)
moran.test(EU$gprb, dnb520.listw, randomisation=TRUE)
moran.test(EU$gprb, dnb620.listw, randomisation=TRUE)
moran.test(EU$gprb, dnb720.listw, randomisation=TRUE) 

#To apply the test to the growth rate with different specifications 
#of the spatial weights matrix (under permutation bootstrap), type
moran.mc(EU$gprb, dnb379.listw, nsim=999)
moran.mc(EU$gprb, dnb420.listw, nsim=999)
moran.mc(EU$gprb, dnb520.listw, nsim=999)
moran.mc(EU$gprb, dnb620.listw, nsim=999)
moran.mc(EU$gprb, dnb720.listw, nsim=999)
##########################################
##########################################

##########################################
### 3. The Moran's I test of spatial autocorrelation in OLS residuals 

#The Moran's I test can also be used as a diagnostic tool to detect 
#the presence of spatial autocorrelation in the residuals of a linear regression model. 
#As an example, run the linear regression of the regional economic convergence in EU2, 

LinearSolow <- lm(gprb ~ log(pr80b) + lninv1b + lnagrib+ lndens_emp, EU)
summary(LinearSolow) 

#The plot of the studentized residuals can give a hint 
#about the presence of spatial dependence in the residuals,
EU$studres <- rstudent(LinearSolow)
tm_shape(EU) + 
  tm_polygons("studres", fill.scale = tm_scale_intervals(style="quantile", n=4)) 

#The command that allows to perform the Moran's I test in the OLS residuals 
#is the function lm.morantest(). To apply the test to the studentized residuals 
#of the linear Solow model, for different specifications of the spatial weights matrix, type
lm.morantest(LinearSolow,dnb379.listw,resfun=rstudent)
lm.morantest(LinearSolow,dnb420.listw,resfun=rstudent)
lm.morantest(LinearSolow,dnb520.listw,resfun=rstudent)
lm.morantest(LinearSolow,dnb620.listw,resfun=rstudent)
lm.morantest(LinearSolow,dnb720.listw,resfun=rstudent)
##########################################
##########################################
