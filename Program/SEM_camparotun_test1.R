#' # Path analysis for phenology project


remove(list = ls())

#' Load Libraries
library(lavaan)
library(sem)
library(lavaanPlot)
library(modelsummary)

#' Load Data
campa.rotun <- read.csv( file = "Data/campa.rotun.csv", 
                header = T, stringsAsFactors = F)
str(campa.rotun)


#' ### Path analysis model specification
model<-'
SPDX ~ 1+ C*AGDU + c*TSNOW
FFD ~ 1+  d*SPDX + f*AGDU
#estimtating the variances of the exogenous variables 
TSNOW ~~ TSNOW
AGDU ~~ AGDU
#estimtating the covariances of the exogenous variables (ses, mastery,performance)
TSNOW ~~ AGDU
#estimating the residual variances for endogenous variables (interest, anxiety, achieve)
SPDX ~~ SPDX
FFD ~~ FFD
#Indirect effects of TSNOW on FFD
TSNOWie2:= 1+ c*d
#Indirect effects of AGDU on FFD
AGDUie2:= 1+ C*d
AGDUiet:= 1 + AGDUie2 + f
#Indirect effect of SPDX on FFD
TSNOW ~ 1
AGDU ~ 1'

#' ### Lavaan function
fit<-lavaan(model,data=campa.rotun, missing = "fiml")
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

# ezknitr::ezspin(file = "Program/SEM_camparotun.R", out_dir = "Output", keep_rmd = F, keep_md = F)


#https://nmmichalak.github.io/nicholas_michalak/blog_entries/2018/nrg01/nrg01.html 