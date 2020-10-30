#' # Path analysis for phenology project: Ranunculus rhomboideus 


remove(list = ls())

#' Load Libraries
library(lavaan)
library(sem)
library(lavaanPlot)
library(modelsummary)
library(grid)
library(lattice)
library(gridExtra)

#' Load Data
achil.mille <- read.csv( file = "Data/achil.mille.csv", header = T, stringsAsFactors = F)
amorp.canes <- read.csv( file = "Data/amorp.canes.csv", header = T, stringsAsFactors = F)
anemo.canad <- read.csv( file = "Data/anemo.canad.csv", header = T, stringsAsFactors = F)
anemo.paten <- read.csv( file = "Data/anemo.paten.csv", header = T, stringsAsFactors = F)
calth.palus <- read.csv( file = "Data/calth.palus.csv", header = T, stringsAsFactors = F)
campa.rotun <- read.csv( file = "Data/campa.rotun.csv", header = T, stringsAsFactors = F)
ceras.arven <- read.csv( file = "Data/ceras.arven.csv", header = T, stringsAsFactors = F)
cypri.candi <- read.csv( file = "Data/cypri.candi.csv", header = T, stringsAsFactors = F)
litho.canes <- read.csv( file = "Data/litho.canes.csv", header = T, stringsAsFactors = F)
oenot.nutta <- read.csv( file = "Data/oenot.nutta.csv", header = T, stringsAsFactors = F)
oxali.viola <- read.csv( file = "Data/oxali.viola.csv", header = T, stringsAsFactors = F)
oxytr.lambe <- read.csv( file = "Data/oxytr.lambe.csv", header = T, stringsAsFactors = F)
pedic.canad <- read.csv( file = "Data/pedic.canad.csv", header = T, stringsAsFactors = F)
penst.graci <- read.csv( file = "Data/penst.graci.csv", header = T, stringsAsFactors = F)
penst.grand <- read.csv( file = "Data/penst.grand.csv", header = T, stringsAsFactors = F)
ranun.rhomb <- read.csv( file = "Data/ranun.rhomb.csv", header = T, stringsAsFactors = F)
rosa.arkan <- read.csv( file = "Data/rosa.arkan.csv", header = T, stringsAsFactors = F)
sisy.angus <- read.csv( file = "Data/sisy.angus.csv", header = T, stringsAsFactors = F)
vicia.ameri <- read.csv( file = "Data/vicia.ameri.csv", header = T, stringsAsFactors = F)
zigad.elaga <- read.csv( file = "Data/zigad.elaga.csv", header = T, stringsAsFactors = F)
zizia.aurea <- read.csv( file = "Data/zizia.aurea.csv", header = T, stringsAsFactors = F)

#' ### Path analysis model specification
model<-'
# DOBG is predicted by TSNOW, AGDU, and SPDX
DOBG ~ 1+ a*TSNOW + A*AGDU
FFD ~ 1+ b*DOBG + f*AGDU
#estimtating the variances of the exogenous variables 
TSNOW ~~ TSNOW
AGDU ~~ AGDU
#estimtating the covariances of the exogenous variables (ses, mastery,performance)
TSNOW ~~ AGDU
#estimating the residual variances for endogenous variables (interest, anxiety, achieve)
DOBG ~~ DOBG
FFD ~~ FFD
#Indirect effects of TSNOW on FFD
TSNOWie1:= 1+ a*b
#Indirect effects of AGDU on FFD
AGDUie1:= 1+ A*b
TSNOW ~ 1
AGDU ~ 1'

#' ### Lavaan function
AM_fit<-lavaan(model,data=achil.mille, missing = "fiml")
AC_fit<-lavaan(model,data=amorp.canes, missing = "fiml")
AnC_fit<-lavaan(model,data=anemo.canad, missing = "fiml")
AP_fit<-lavaan(model,data=anemo.paten, missing = "fiml")
CP_fit<-lavaan(model,data=calth.palus, missing = "fiml")
CR_fit<-lavaan(model,data=campa.rotun, missing = "fiml")
CA_fit<-lavaan(model,data=ceras.arven, missing = "fiml")
CC_fit<-lavaan(model,data=cypri.candi, missing = "fiml")
LC_fit<-lavaan(model,data=litho.canes, missing = "fiml")
ON_fit<-lavaan(model,data=oenot.nutta, missing = "fiml")
OV_fit<-lavaan(model,data=oxali.viola, missing = "fiml")
OL_fit<-lavaan(model,data=oxytr.lambe, missing = "fiml")
PC_fit<-lavaan(model,data=pedic.canad, missing = "fiml")
PG_fit<-lavaan(model,data=penst.graci, missing = "fiml")
PeG_fit<-lavaan(model,data=penst.grand, missing = "fiml")
RR_fit<-lavaan(model,data=ranun.rhomb, missing = "fiml")
RA_fit<-lavaan(model,data=rosa.arkan, missing = "fiml")
SA_fit<-lavaan(model,data=sisy.angus, missing = "fiml")
VA_fit<-lavaan(model,data=vicia.ameri, missing = "fiml")
ZE_fit<-lavaan(model,data=zigad.elaga, missing = "fiml")
ZA_fit<-lavaan(model,data=zizia.aurea, missing = "fiml")



#' ###  path plots
AM_PLOT <- lavaanPlot(model = AM_fit, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), 
           coefs = TRUE,covs=TRUE,stars = c("regress"))
AC_PLOT <- lavaanPlot(model = AC_fit, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), 
           coefs = TRUE,covs=TRUE,stars = c("regress"))
lavaanPlot(model = AnC_fit, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), 
           coefs = TRUE,covs=TRUE,stars = c("regress"))
lavaanPlot(model = AP_fit, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), 
           coefs = TRUE,covs=TRUE,stars = c("regress"))
lavaanPlot(model = CP_fit, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), 
           coefs = TRUE,covs=TRUE,stars = c("regress"))
lavaanPlot(model = CR_fit, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), 
           coefs = TRUE,covs=TRUE,stars = c("regress"))
lavaanPlot(model = CA_fit, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), 
           coefs = TRUE,covs=TRUE,stars = c("regress"))
lavaanPlot(model = CC_fit, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), 
           coefs = TRUE,covs=TRUE,stars = c("regress"))
lavaanPlot(model = LC_fit, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), 
           coefs = TRUE,covs=TRUE,stars = c("regress"))
lavaanPlot(model = ON_fit, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), 
           coefs = TRUE,covs=TRUE,stars = c("regress"))
lavaanPlot(model = OV_fit, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), 
           coefs = TRUE,covs=TRUE,stars = c("regress"))
lavaanPlot(model = PC_fit, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), 
           coefs = TRUE,covs=TRUE,stars = c("regress"))
lavaanPlot(model = PG_fit, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), 
           coefs = TRUE,covs=TRUE,stars = c("regress"))
lavaanPlot(model = PeG_fit, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), 
           coefs = TRUE,covs=TRUE,stars = c("regress"))
lavaanPlot(model = RR_fit, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), 
           coefs = TRUE,covs=TRUE,stars = c("regress"))
lavaanPlot(model = RA_fit, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), 
           coefs = TRUE,covs=TRUE,stars = c("regress"))
lavaanPlot(model = SA_fit, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), 
           coefs = TRUE,covs=TRUE,stars = c("regress"))
lavaanPlot(model = VA_fit, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), 
           coefs = TRUE,covs=TRUE,stars = c("regress"))
lavaanPlot(model = ZE_fit, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), 
           coefs = TRUE,covs=TRUE,stars = c("regress"))
lavaanPlot(model = ZA_fit, node_options = list(shape = "box", fontname = "Helvetica"), 
           edge_options = list(color = "grey"), 
           coefs = TRUE,covs=TRUE,stars = c("regress"))

grid.arrange(arrangeGrob(AM_PLOT, AC_PLOT,
                         ncol=1, nrow=2, 
                         bottom = textGrob("TSNOW", vjust = 1),
                         left = textGrob("FFD", rot = 90, vjust = 1)))
