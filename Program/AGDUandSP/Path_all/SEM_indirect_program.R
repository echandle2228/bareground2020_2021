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
DOBG ~ a*TSNOW + A*AGDU + e*SPDX
SPDX ~ C*AGDU + c*TSNOW
FFD ~ b*DOBG + d*SPDX + f*AGDU
#estimtating the variances of the exogenous variables 
TSNOW ~~ TSNOW
AGDU ~~ AGDU
#estimtating the covariances of the exogenous variables (ses, mastery,performance)
TSNOW ~~ AGDU
#estimating the residual variances for endogenous variables (interest, anxiety, achieve)
DOBG ~~ DOBG
SPDX ~~ SPDX
FFD ~~ FFD
#Indirect effects of TSNOW on FFD
TSNOWie1:= a*b
TSNOWie2:= c*d
TSNOWiet:= TSNOWie1 + TSNOWie2
#Indirect effects of AGDU on FFD
AGDUie1:= A*b
AGDUie2:= C*d
AGDUiet:= AGDUie1 + AGDUie2 + f
#Indirect effect of SPDX on FFD
SPDXie1:= e*b'


#using naive bootstrap to obtain standard errors
fit<-lavaan(model,data=ranun.rhomb,se="bootstrap")
summary(fit,fit.measures=TRUE)

#using 'parameterEstimates' function will give us confidence intervals based
#on naive bootstrap. A standard approach to testing indirect effects.



summary(fit,fit.measures=TRUE)

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
