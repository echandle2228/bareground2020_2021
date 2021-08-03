#' # Path analysis for phenology project: Achillea millefolium


remove(list = ls())

#' Load Libraries
library(lavaan)
library(sem)
library(lavaanPlot)
library(modelsummary)

#' Load Data
penst.grand <- read.csv( file = "Data/penst.grand.csv", 
                         header = T, stringsAsFactors = F)
str(penst.grand)


#' ### Path analysis model specification
model<-'
# DOBG is predicted by TSNOW, ST, and SPDX
DOBG ~ 1+ a*TSNOW + A*ST + e*SPDX
SPDX ~ 1+ C*ST + c*TSNOW
FFD ~ 1+ b*DOBG + d*SPDX + f*ST
#estimtating the variances of the exogenous variables 
TSNOW ~~ TSNOW
ST ~~ ST
#estimtating the covariances of the exogenous variables (ses, mastery,performance)
TSNOW ~~ ST
#estimating the residual variances for endogenous variables (interest, anxiety, achieve)
DOBG ~~ DOBG
SPDX ~~ SPDX
FFD ~~ FFD
#Indirect effects of TSNOW on FFD
TSNOWie1:= 1+ a*b
TSNOWie2:= 1+ c*d
TSNOWiet:= 1+ TSNOWie1 + TSNOWie2
#Indirect effects of ST on FFD
STie1:= 1+ A*b
STie2:= 1+ C*d
STiet:= 1+ STie1 + STie2 + f
#Indirect effect of SPDX on FFD
SPDXie1:= 1+ e*b
TSNOW ~ 1
ST ~ 1'

#' ### Lavaan function
fit<-lavaan(model,data=penst.grand, missing = "fiml")
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


# ezknitr::ezspin(file = "Program/STandSPDX/Path_all/SEM_penstgrand_STandSPDX.R", out_dir = "Output", keep_rmd = F, keep_md = F)


#https://nmmichalak.github.io/nicholas_michalak/blog_entries/2018/nrg01/nrg01.html 