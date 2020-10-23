# SEM for phenology project 
# Data for Ranun rhom
remove(list = ls())

# Load Libraries
library(lavaan)
library(sem)
library(lavaanPlot)

# Load Data
ranun.rhomb <- read.csv( file = "Data/ranun.rhomb.csv", 
                header = T, stringsAsFactors = F)
str(ranun.rhomb)


#model specification

model<-'
# DOBG is predicted by TSNOW, AGDU, and SPDX
DOBG ~ 1+ TSNOW + AGDU + SPDX
SPDX ~ 1+ AGDU + TSNOW
FFD ~ 1+ DOBG + SPDX + AGDU
#estimtating the variances of the exogenous variables 
TSNOW ~~ TSNOW
AGDU ~~ AGDU
#estimtating the covariances of the exogenous variables (ses, mastery,performance)
TSNOW ~~ AGDU
#estimating the residual variances for endogenous variables (interest, anxiety, achieve)
DOBG ~~ DOBG
SPDX ~~ SPDX
FFD ~~ FFD
TSNOW ~ 1
AGDU ~ 1'


fit<-lavaan(model,data=ranun.rhomb, missing = "fiml")


summary(fit,fit.measures=TRUE)

library(semPlot)
semPaths(fit)

#Standardized Measurements
summary(fit,fit.measures=TRUE,standardized=TRUE,rsquare=TRUE)
#Confidence Intervals
parameterEstimates(fit)

#Comprehensive set of fit measures
fitMeasures(fit)

#Modification indicies
modificationIndices(fit)


lavaanPlot(model = fit, node_options = list(shape = "box", fontname = 
                                              "Helvetica"), edge_options = list(color = "grey"), 
           coefs = TRUE,covs=
             TRUE,stars = c("regress"))
