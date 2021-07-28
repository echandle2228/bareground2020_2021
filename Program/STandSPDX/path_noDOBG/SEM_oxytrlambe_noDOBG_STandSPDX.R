remove(list = ls())

#' Load Libraries
library(lavaan)
library(sem)
library(lavaanPlot)
library(modelsummary)

#' Load Data
oxytr.lambe <- read.csv( file = "Data/oxytr.lambe.csv", 
                         header = T, stringsAsFactors = F)
str(oxytr.lambe)


#' ### Path analysis model specification
model<-'
SPDX ~ 1+ C*ST + c*TSNOW
FFD ~ 1+  d*SPDX + f*ST
#estimtating the variances of the exogenous variables 
TSNOW ~~ TSNOW
ST ~~ ST
#estimtating the covariances of the exogenous variables (ses, mastery,performance)
TSNOW ~~ ST
#estimating the residual variances for endogenous variables (interest, anxiety, achieve)
SPDX ~~ SPDX
FFD ~~ FFD
#Indirect effects of TSNOW on FFD
TSNOWie2:= c*d
#Indirect effects of ST on FFD
STie2:= C*d
STiet:= STie2 + f
#Indirect effect of SPDX on FFD
TSNOW ~ 1
ST ~ 1'

#' ### Lavaan function
fit<-lavaan(model,data=oxytr.lambe, missing = "fiml")
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

#' ### Path plots
lavaanPlot(model = fit, node_options = list(shape = "box", fontname = 
                                              "serif"), edge_options = list(color = "grey"), 
           coefs = TRUE, stand = TRUE,covs=
             TRUE,stars = c("regress"))



library(semPlot)
semPaths(fit)

# ezknitr::ezspin(file = "Program/STandSPDX/path_noDOBG/SEM_oxytrlambe_noDOBG_STandSPDX.R", out_dir = "Output", keep_rmd = F, keep_md = F)


#https://nmmichalak.github.io/nicholas_michalak/blog_entries/2018/nrg01/nrg01.html 