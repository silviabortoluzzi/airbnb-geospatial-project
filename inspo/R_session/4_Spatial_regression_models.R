
####################################
### 4. Spatial regression models ###
####################################

#In this lab we deal with the three main specifications 
#of linear spatial regression models that can be considered 
#once the hypothesis of no spatial autocorrelation 
#in the OLS residuals is violated
#
# The spatial durbin model SDM 
# The spatial autoregressive model SAR 
# The spatial durbin error model SDEM 
# The spatial error model SEM 
# The lagged independent variable model LDM 

#Throughout this lab we will use the data about the regional growth 
#in the European Union area,
setwd("...")
library(sf); library(tmap); library(spdep)
library(spatialreg)# "spatialreg" is the package containing functions to estimate spatial regression models 

EU2 <- st_read("EU2")
coords <- st_centroid(st_geometry(EU2))
dnb420 <- dnearneigh(coords, 0, 420) #neighbourhood relationships definition
dnb420.listw <- nb2listw(dnb420,style="W") #Spatial weights definition

##########################################
### 1. Models Estimation

#To estimate the SDM model using the Maximum likelihood estimator
#we can use the function "lagsarlm" with specification "Durbin=T",
SDM <- lagsarlm(gprb~log(pr80b)+lninv1b+lnagrib+lndens_emp, data = EU2, listw=dnb420.listw,
                Durbin=T)
summary(SDM)

#To estimate the SAR model using the Maximum likelihood estimator
#we can use the function "lagsarlm",
SAR <- lagsarlm(gprb~log(pr80b)+lninv1b+lnagrib+lndens_emp, data = EU2, listw=dnb420.listw)
summary(SAR)

#To estimate the SDEM model using the Maximum likelihood estimator
#we can use the function "errorsarlm" with specification "Durbin=T",
SDEM <- errorsarlm(gprb~log(pr80b)+lninv1b+lnagrib+lndens_emp, data = EU2, listw=dnb420.listw,
                   Durbin=T)
summary(SDEM)

#To estimate the SEM model using the Maximum likelihood estimator
#we can use the function "errorsarlm",
SEM <- errorsarlm(gprb~log(pr80b)+lninv1b+lnagrib+lndens_emp, data = EU2, listw=dnb420.listw)
summary(SEM)

#To estimate the LSX model using the OLS estimator
#we can use the function "lmSLX", the argument Durbin can be used to specify 
#the subset of explanatory variables to lag
SLX <- lmSLX(gprb~log(pr80b)+lninv1b+lnagrib+lndens_emp, data = EU2, listw=dnb420.listw)
summary(SLX)
##########################################
##########################################

##########################################
### 2. Interpreting parameters and testing for spillovers

###Interpretation
#From Arbia (2014):
#In a standard linear regression model the regression parameters have an easy interpretation
#in that they represent the partial derivative of the dependent variable with respect to the
#independent variables.
#A regression coefficient of an independent variable can therefore be straightforwardly 
#interpreted as the variation induced on the dependent variable of a unitary increase 
#in the single independent variable.
#However, in SAR and SDM models the interpretation of the parameters is less immediate 
#and requires some clarifications. In fact, a variation of an independent variable X 
#observed in location i has not only an effect on the value of the dependent variable y 
#in the same location, but also on variable y observed in other locations.

#The formal solution to the problem consists in evaluating the partial derivative.
#On this basis the following impact measures can be
#calculated for each independent variable X included in the model:
#  Average Direct Impact
#  Average Indirect impact
#  Average Total impact

#Impact measures can be computed using the function "impacts"
#For the SAR specification:
impSAR <- impacts(SAR, listw=dnb420.listw, R=100)
summary(impSAR, zstats=TRUE, short=TRUE)

#For the SD specification:
impSDM <- impacts(SDM, listw=dnb420.listw, R=100)
summary(impSDM, zstats=TRUE, short=TRUE)
##########################################
##########################################

##########################################
### 3. Choosing the proper specification

###The Lagrange multiplier (LM) test of spatial dependence on OLS residuals
#In the LM test the alternative hypothesis is explicitly considered to contrast the null
#of absence of spatial dependence.
#In particular, we can explicitly express the alternative hypothesis either in the form 
#of a SL or of a SEM

#The LM test can be computed using the function "lm.RStests"
OLSmodel <- lm(gprb~log(pr80b)+lninv1b+lnagrib+lndens_emp, data = EU2)
natOLSlmTests <- lm.RStests(OLSmodel, dnb420.listw, 
                    test=c("RSerr", "RSlag", "adjRSerr", "adjRSlag"))
summary(natOLSlmTests)

###The selection strategy proposed by Elhorst (2010): 
# 1 - Estimate the OLS model and test (with the LM test) whether 
      #the SL or the SEM is more appropriate to describe the data
# 2 - If the OLS model is rejected in favour of the SL, the SEM
      #or in favour of both models, then the SDM should be estimated
# 3 - likelihood ratio (LR) tests can subsequently be used to test whether 
      #i) the SDM can be simplified to the SLM, 
      #and ii) whether it can be simplified to the SEM.

#If both hypotheses are rejected, then the SDM best describes the data. 

#If the hypothesis i) cannot be rejected, then the SLM best describes the data, 
#provided that the (robust) LM tests also pointed to the SLM

#If the hypothesis ii) cannot be rejected, then the SEM best describes the data,
#provided that the (robust) LM tests also pointed to the SEM
  
#We can perform LR tests of restrictions on the parameters of spatial models
#using the function "anova"
#To test hypothesis i), type 
anova(SDM, SAR)

#To test hypothesis ii), type 
anova(SDM, SEM)

#according to the tests, the SEM may be the proper specification.
#Therefore, estimate the SDEM and verify whether the parameters of the 
#lagged indpendent variables are significant 
anova(SDEM, SEM)

#However, one may use SDM anyway since it produces correct
#standard errors or t-values of the coefficient estimates also 
#if the true data generating process is a SEM








