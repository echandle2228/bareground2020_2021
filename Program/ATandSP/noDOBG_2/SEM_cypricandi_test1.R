#' # Path analysis for phenology project


remove(list = ls())

#' Load Libraries
library(lavaan)
library(sem)
library(lavaanPlot)
library(modelsummary)

#' Load Data
cypri.candi <- read.csv( file = "Data/cypri.candi.csv", 
                         header = T, stringsAsFactors = F)
str(cypri.candi)


#' ### Path analysis model specification
model<-'
SP ~ 1+ C*AT + c*TSNOW
FFD ~ 1+  d*SP + f*AT
#estimtating the variances of the exogenous variables 
TSNOW ~~ TSNOW
AT ~~ AT
#estimtating the covariances of the exogenous variables (ses, mastery,performance)
TSNOW ~~ AT
#estimating the residual variances for endogenous variables (interest, anxiety, achieve)
SP ~~ SP
FFD ~~ FFD
#Indirect effects of TSNOW on FFD
TSNOWie2:= c*d
#Indirect effects of AT on FFD
ATie2:= C*d
ATiet:= ATie2 + f
#Indirect effect of SP on FFD
TSNOW ~ 1
AT ~ 1'

#' ### Lavaan function
fit<-lavaan(model,data=cypri.candi, missing = "fiml")
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


# ezknitr::ezspin(file = "Program/path_noDOBG/SEM_cypricandi_test1.R", out_dir = "Output", keep_rmd = F, keep_md = F)


#https://nmmichalak.github.io/nicholas_michalak/blog_entries/2018/nrg01/nrg01.html 