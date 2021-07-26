#' # Path analysis for phenology project: Ranunculus rhomboideus 


remove(list = ls())

#' Load Libraries
library(lavaan)
library(sem)
library(lavaanPlot)
library(modelsummary)

#' Load Data
ranun.rhomb <- read.csv( file = "Data/ranun.rhomb.csv", 
                header = T, stringsAsFactors = F)
str(ranun.rhomb)


#' ### Path analysis model specification
model<-'
# DOBG is predicted by TSNOW, AGDU, and SPDX
DOBG ~ 1+ a*TSNOW + e*SPDX
SPDX ~ 1+ c*TSNOW
FFD ~ 1+ b*DOBG + d*SPDX
#estimtating the variances of the exogenous variables 
TSNOW ~~ TSNOW
#estimating the residual variances for endogenous variables (interest, anxiety, achieve)
DOBG ~~ DOBG
SPDX ~~ SPDX
FFD ~~ FFD
#Indirect effects of TSNOW on FFD
TSNOWie1:= 1+ a*b
TSNOWie2:= 1+ c*d
TSNOWiet:= 1+ TSNOWie1 + TSNOWie2
#Indirect effect of SPDX on FFD
SPDXie1:= 1+ e*b
TSNOW ~ 1'

#' ### Lavaan function
fit<-lavaan(model,data=ranun.rhomb, missing = "fiml")
summary(fit,fit.measures=TRUE)
modelsummary(fit)

#' ### Standardized Measurements
summary(fit,fit.measures=TRUE,standardized=TRUE,rsquare=TRUE)

#' ### Confidence Intervals
parameterEstimates(fit)

#' ### Comprehensive set of fit measures
fitMeasures(fit)

#' ### Modification indicies
modificationIndices(fit)

#' ### Example path plots
lavaanPlot(model = fit, node_options = list(shape = "box", fontname = 
                                              "Helvetica"), edge_options = list(color = "grey"), 
           coefs = TRUE,covs=
             TRUE,stars = c("regress"))


library(semPlot)
semPaths(fit)

# ezknitr::ezspin(file = "Program/SEM_ranunrhomb.R", out_dir = "Output", keep_rmd = F, keep_md = F)


#https://nmmichalak.github.io/nicholas_michalak/blog_entries/2018/nrg01/nrg01.html 