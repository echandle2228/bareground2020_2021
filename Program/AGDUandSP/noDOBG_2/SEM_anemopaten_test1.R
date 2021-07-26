remove(list = ls())

#' Load Libraries
library(lavaan)
library(sem)
library(lavaanPlot)
library(modelsummary)

#' Load Data
anemo.paten <- read.csv( file = "Data/anemo.paten.csv", 
                         header = T, stringsAsFactors = F)
str(anemo.paten)


#' ### Path analysis model specification
model<-'
SP ~ 1+ C*AGDU + c*TSNOW
FFD ~ 1+  d*SP + f*AGDU
#estimtating the variances of the exogenous variables 
TSNOW ~~ TSNOW
AGDU ~~ AGDU
#estimtating the covariances of the exogenous variables (ses, mastery,performance)
TSNOW ~~ AGDU
#estimating the residual variances for endogenous variables (interest, anxiety, achieve)
SP ~~ SP
FFD ~~ FFD
#Indirect effects of TSNOW on FFD
TSNOWie2:= c*d
#Indirect effects of AGDU on FFD
AGDUie2:= C*d
AGDUiet:= AGDUie2 + f
#Indirect effect of SP on FFD
TSNOW ~ 1
AGDU ~ 1'

#' ### Lavaan function
fit<-lavaan(model,data=anemo.paten, missing = "fiml")
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


# ezknitr::ezspin(file = "Program/AGDUandSP/noDOBG_2/SEM_anemopaten_test1.R", out_dir = "Output", keep_rmd = F, keep_md = F)


#https://nmmichalak.github.io/nicholas_michalak/blog_entries/2018/nrg01/nrg01.html 