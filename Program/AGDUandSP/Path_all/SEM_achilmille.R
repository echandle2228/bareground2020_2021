remove(list = ls())

#' Load Libraries
library(lavaan)
library(sem)
library(lavaanPlot)
library(modelsummary)

#' Load Data
achil.mille <- read.csv( file = "Data/achil.mille.csv", 
                header = T, stringsAsFactors = F)
str(achil.mille)


#' ### Path analysis model specification
model<-'
# DOBG is predicted by TSNOW, AGDU, and SP
DOBG ~ 1+ a*TSNOW + A*AGDU + e*SP
SP ~ 1+ C*AGDU + c*TSNOW
FFD ~ 1+ b*DOBG + d*SP + f*AGDU
#estimtating the variances of the exogenous variables 
TSNOW ~~ TSNOW
AGDU ~~ AGDU
#estimtating the covariances of the exogenous variables (ses, mastery,performance)
TSNOW ~~ AGDU
#estimating the residual variances for endogenous variables (interest, anxiety, acAGDUeve)
DOBG ~~ DOBG
SP ~~ SP
FFD ~~ FFD
#Indirect effects of TSNOW on FFD
TSNOWie1:= 1+ a*b
TSNOWie2:= 1+ c*d
TSNOWiet:= 1+ TSNOWie1 + TSNOWie2
#Indirect effects of AGDU on FFD
AGDUie1:= 1+ A*b
AGDUie2:= 1+ C*d
AGDUiet:= 1+ AGDUie1 + AGDUie2 + f
#Indirect effect of SP on FFD
SPie1:= 1+ e*b
TSNOW ~ 1
AGDU ~ 1'

#' ### Lavaan function
fit<-lavaan(model,data=achil.mille, missing = "fiml")
summary(fit,fit.measures=TRUE)
modelsummary(fit)

#' ### Standardized Measurements
summary(fit,fit.measures=TRUE,standardized=TRUE,rsquare=TRUE)
standardizedSolution(fit)

#' ### Confidence Intervals
parameterEstimates(fit)

#' ### Comprehensive set of fit measures
fitMeasures(fit)

#' ### Modification indicies
modificationIndices(fit)

#' ### Example path plots
lavaanPlot(model = fit, node_options = list(shape = "box", fontname = 
                                              "serif"), edge_options = list(color = "grey"), 
           coefs = TRUE, stand = TRUE,covs=
             TRUE,stars = c("regress"))

ezknitr::ezspin(file = "Program/AGDUandSP/Path_all/SEM_achilmille.R", out_dir = "Output", keep_rmd = F, keep_md = F)


#https://nmmichalak.github.io/nicholas_michalak/blog_entries/2018/nrg01/nrg01.html 