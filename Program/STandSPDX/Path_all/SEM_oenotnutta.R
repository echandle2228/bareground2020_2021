#' # Path analysis for phenology project: Oenothera nuttallii


remove(list = ls())

#' Load Libraries
library(lavaan)
library(sem)
library(lavaanPlot)
library(modelsummary)

#' Load Data
oenot.nutta <- read.csv( file = "Data/oenot.nutta.csv", 
                header = T, stringsAsFactors = F)
str(oenot.nutta)


#' ### Path analysis model specification
model<-'
# DOBG is predicted by TSNOW, AGDU, and SPDX
DOBG ~ 1+ a*TSNOW + A*AGDU + e*SPDX
SPDX ~ 1+ C*AGDU + c*TSNOW
FFD ~ 1+ b*DOBG + d*SPDX + f*AGDU
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
TSNOWie1:= 1+ a*b
TSNOWie2:= 1+ c*d
TSNOWiet:= 1+ TSNOWie1 + TSNOWie2
#Indirect effects of AGDU on FFD
AGDUie1:= 1+ A*b
AGDUie2:= 1+ C*d
AGDUiet:= 1+ AGDUie1 + AGDUie2 + f
#Indirect effect of SPDX on FFD
SPDXie1:= 1+ e*b
TSNOW ~ 1
AGDU ~ 1'

#' ### Lavaan function
fit<-lavaan(model,data=oenot.nutta, missing = "fiml")
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

# ezknitr::ezspin(file = "Program/SEM_oenotnutta.R", out_dir = "Output", keep_rmd = F, keep_md = F)


#https://nmmichalak.github.io/nicholas_michalak/blog_entries/2018/nrg01/nrg01.html 