remove(list = ls())

#' Load Libraries
library(lavaan)
library(sem)
library(lavaanPlot)
library(modelsummary)

#' Load Data
anemo.canad <- read.csv( file = "Data/anemo.canad.csv", 
                         header = T, stringsAsFactors = F)
str(anemo.canad)


#' ### Path analysis model specification
model<-'
# DOBG is predicted by TSNOW, HI, and SP
DOBG ~ 1+ a*TSNOW + A*HI
FFD ~ 1+ b*DOBG + f*HI
#estimtating the variances of the exogenous variables 
TSNOW ~~ TSNOW
HI ~~ HI
#estimtating the covariances of the exogenous variables (ses, mastery,performance)
TSNOW ~~ HI
#estimating the residual variances for endogenous variables (interest, anxiety, achieve)
DOBG ~~ DOBG
FFD ~~ FFD
#Indirect effects of TSNOW on FFD
TSNOWie1:= 1+ a*b
#Indirect effects of HI on FFD
HIie1:= 1+ A*b
TSNOW ~ 1
HI ~ 1'

#' ### Lavaan function
fit<-lavaan(model,data=anemo.canad, missing = "fiml")
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

# ezknitr::ezspin(file = "Program/noSP_2/SEM_anemocanad_test2.R", out_dir = "Output", keep_rmd = F, keep_md = F)


#https://nmmichalak.github.io/nicholas_michalak/blog_entries/2018/nrg01/nrg01.html 