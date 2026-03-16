
#########################################################################
### 3. Descriptive spatial statistics for areal data (local analysis) ###
#########################################################################

#The Moran's I statistic is a global measure and hence does not allow to identify 
#the local patterns of spatial autocorrelation. 
#In many circumstances, however, it may be also interesting to assess 
#the presence of local spatial clusters and verify the specific contribution 
#of each particular region to the global pattern of spatial dependence. 
#Local spatial autocorrelation can be investigated by the means 
#of the Moran scatterplot and the local Moran's I.  

#Throughout this lab we will use the data about the regional growth 
#in the European Union area,
setwd("...")
library(spdep); library(sf); library(tmap)
EU2 <- st_read("EU2")

##########################################
### 1. Moran scatterplot  

#The Moran scatterplot is a graph which plots the variable of interest, x, 
#on the horizontal axis and its corresponding spatially lagged values, namely wx, 
#on the vertical axis. 
#The four quadrants of the Moran scatterplot connote the four possible kinds 
#of spatial association between each region and the other neighbouring regions. 
#In particular, the quadrant "High-High" identifies the regions with a high 
#(above the average) value of x and also a high (above the average) value of wx. 
#On the other hand, the quadrant "Low-Low" identifies the regions with a low 
#(below the average) value of x and also a low (below the average) value of wx. 
#The regions which fall into the High-High and Low-Low quadrants are characterized 
#by positive spatial autocorrelation and are surrounded by regions with similar values.  
#Conversely, the two remaining quadrants, "High-Low" and "Low-High" identify 
#local patterns of negative spatial autocorrelation since they collect 
#the regions with a high (respectively low) value of x and, in opposition, 
#a low (respectively high) value of wx. These regions are surrounded by other regions 
#with dissimilar values.        

#The command to plot the Moran scatterplot is the function "moran.plot", 
#which requires the user to specify at least the variable of interest 
#and the spatial weight matrix. 
#To create the Moran scatterplot for growth rates of production per capita 
#according to a spatial weight matrix based on a critical cut-off distance of 321 Km, 
#type
coords <- st_centroid(st_geometry(EU2))
dnb321 <- dnearneigh(coords, 0, 321)
dnb321.listw <- nb2listw(dnb321,style="W",zero.policy=F)
mplot <- moran.plot(EU2$gprb, listw=dnb321.listw, main="Moran scatterplot")
grid()

#The regions which are relatively more influential than the others in determining 
#the observed value of global Moran's I are represented by marked points. 
#These regions are identified as those which exert the most influence on 
#the regression line on the basis of standard criteria 
#such as the Cook's distance and leverages.

#It may be useful to map regions according to their hat value influence measure
EU2$hat_value <- mplot$hat
tm_shape(EU2) + tm_polygons("hat_value")

#It may be also useful to map regions with noteworthy influence coded by their 
#quadrant in the Moran scatterplot. 
#First of all, we need to identify the influential regions, 
#which can be done by typing 
mplot <- moran.plot(EU2$gprb, listw=dnb321.listw, main="Moran scatterplot", 
         return_df=F)
hotspot <- as.numeric(row.names(as.data.frame(summary(mplot))))

#Then, using function localmoran(), we can obtain the Moran scatterplot quadrants
EU2$quadrant <- attr(localmoran(EU2$gprb, dnb321.listw), "quadr")$mean
EU2$quadrant[-hotspot] <- NA
table(EU2$quadrant)

#which allows us to plot the map of the regions with influence by typing
tm_shape(EU2) + tm_polygons("quadrant") 
##########################################
##########################################

##########################################
### 2. The Local Moran's I 

#The Moran scatterplot represents a useful and intuitive visual representation 
#of the local patterns of spatial association but cannot provide 
#the statistical significance of the results. 
#As a consequence, in order to assess the significance of the revealed pattern, 
#we may rely on the so-called local Moran's I index.

#The command that allows to compute the local Moran's I index values 
#and their significance values is the function localmoran(). 

#To compute the index for the growth rate with a spatial weight matrix 
#based on a critical cut-off distance of 321 Km, type 
lmI <- localmoran(EU2$gprb, dnb321.listw)
head(lmI)

#The distribution of the local Moran's I index values 
#may be represented graphically typing 
EU2$lmI <- lmI[,1]
tm_shape(EU2) + 
    tm_polygons("lmI", fill.legend = tm_legend(title="Local Moran's I values")) 


#As with the global index, the local Moran's I's statistics can be tested for 
#deviations using the hypothesis of absence of local spatial autocorrelation 
#and hence can provide the statistical significance of the local spatial patterns 
#detected by the Moran scatterplot. 
#In particular, to map the corresponding p-values we may type:
EU2$locmpv <- p.adjust(lmI[, "Pr(z != E(Ii))"], "bonferroni")
tm_shape(EU2) + 
    tm_polygons("locmpv", 
                fill.scale = tm_scale(c(0, 0.0005, 0.001, 0.005, 0.01, 
                                        0.05, 0.1, 0.2, 0.5, 0.75, 1)), 
                fill.legend = tm_legend(title="Local Moran's I significance map")) 

#To perform bootstrap-based inference, conditional permutation should be used, 
#fixing the value at observation and randomly sampling from the remaining values to find 
#randomised values at neighbours, and is provided as localmoran_perm()

lmIp <- localmoran_perm(EU2$gprb, dnb321.listw, nsim = 9999, iseed = 1) 
EU2$locmpvPerm <- p.adjust(lmIp[, "Pr(z != E(Ii)) Sim"], "bonferroni")
tm_shape(EU2) + 
    tm_polygons("locmpvPerm", 
                fill.scale = tm_scale(c(0, 0.0005, 0.001, 0.005, 0.01, 
                                        0.05, 0.1, 0.2, 0.5, 0.75, 1)), 
                fill.legend = tm_legend(title="Local Moran's I significance map"))

#Moran's I p-values can be used to assess the significance of hotspots 
#A typical choice is p-value < 0.005
EU2$quadrant2 <- attr(lmI, "quadr")$mean
EU2$quadrant2[EU2$locmpv>0.05] <- NA
tm_shape(EU2) + tm_polygons("quadrant2") 
##########################################
##########################################
  