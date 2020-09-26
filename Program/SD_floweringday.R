# Finding day in March with snow depth that is the best predictor of first flowering day

# Load libraries

library(writexl)

remove(list = ls())
set.seed(788843)


SD <- read.csv( file = "Data/March_snow_noyear.csv", 
                     header = T, stringsAsFactors = F)
P1 <- read.csv( file = "Data/common_species.csv",
                header = T, stringsAsFactors = F)
bareday <- read.csv( file = "Data/bareday.csv",
                     header = T, stringsAsFactors = F)


# 
# > SD <- read.delim("clipboard")
# > P1 <- read.delim("clipboard")
# > attach(SD)
# > attach(P1)


# create storage matrix for AIC values
snowdim <- dim(SD)[2]

#loop through all 'i' possible DU sets and extract AIC values
#calculate delta-AICs
#best fitting model
#which SD is it?

# Oxali.viola values
AIC.Oxali.viola <- matrix(NA,snowdim)  
for(i in 1:snowdim){AIC.Oxali.viola[i,] <- AIC(lm(P1$Oxali.viola~SD[,i]))}  
DeltaAIC.Oxali.viola<-AIC.Oxali.viola-min(AIC.Oxali.viola) 
best.Oxali.viola <- which(DeltaAIC.Oxali.viola==0, arr.ind = T)  
colnames(SD)[best.Oxali.viola[1]] 

# Ranun.rhomb values
AIC.Ranun.rhomb <- matrix(NA,snowdim)  
for(i in 1:snowdim){AIC.Ranun.rhomb[i,] <- AIC(lm(P1$Ranun.rhomb~SD[,i]))}  
DeltaAIC.Ranun.rhomb<-AIC.Ranun.rhomb-min(AIC.Ranun.rhomb) 
best.Ranun.rhomb <- which(DeltaAIC.Ranun.rhomb==0, arr.ind = T)
colnames(SD)[best.Ranun.rhomb[1]] 

# Ceras.arven values
AIC.Ceras.arven <- matrix(NA,snowdim)  
for(i in 1:snowdim){AIC.Ceras.arven[i,] <- AIC(lm(P1$Ceras.arven~SD[,i]))}  
DeltaAIC.Ceras.arven<-AIC.Ceras.arven-min(AIC.Ceras.arven) 
best.Ceras.arven <- which(DeltaAIC.Ceras.arven==0, arr.ind = T)
colnames(SD)[best.Ceras.arven[1]] 

# Sisyr.angus values
AIC.Sisyr.angus <- matrix(NA,snowdim)  
for(i in 1:snowdim){AIC.Sisyr.angus[i,] <- AIC(lm(P1$Sisyr.angus~SD[,i]))}  
DeltaAIC.Sisyr.angus<-AIC.Sisyr.angus-min(AIC.Sisyr.angus) 
best.Sisyr.angus <- which(DeltaAIC.Sisyr.angus==0, arr.ind = T)
colnames(SD)[best.Sisyr.angus[1]] 

# Anemo.paten values
AIC.Anemo.paten <- matrix(NA,snowdim)  
for(i in 1:snowdim){AIC.Anemo.paten[i,] <- AIC(lm(P1$Anemo.paten~SD[,i]))}  
DeltaAIC.Anemo.paten<-AIC.Anemo.paten-min(AIC.Anemo.paten) 
best.Anemo.paten <- which(DeltaAIC.Anemo.paten==0, arr.ind = T)
colnames(SD)[best.Anemo.paten[1]] 

# Anemo.canad values
AIC.Anemo.canad <- matrix(NA,snowdim)  
for(i in 1:snowdim){AIC.Anemo.canad[i,] <- AIC(lm(P1$Anemo.canad~SD[,i]))}  
DeltaAIC.Anemo.canad<-AIC.Anemo.canad-min(AIC.Anemo.canad) 
best.Anemo.canad <- which(DeltaAIC.Anemo.canad==0, arr.ind = T)
colnames(SD)[best.Anemo.canad[1]] 

# Penst.graci values
AIC.Penst.graci <- matrix(NA,snowdim)  
for(i in 1:snowdim){AIC.Penst.graci[i,] <- AIC(lm(P1$Penst.graci~SD[,i]))}  
DeltaAIC.Penst.graci<-AIC.Penst.graci-min(AIC.Penst.graci) 
best.Penst.graci <- which(DeltaAIC.Penst.graci==0, arr.ind = T)
colnames(SD)[best.Penst.graci[1]] 

# Ranun.abort values
AIC.Ranun.abort <- matrix(NA,snowdim)  
for(i in 1:snowdim){AIC.Ranun.abort[i,] <- AIC(lm(P1$Ranun.abort~SD[,i]))}  
DeltaAIC.Ranun.abort<-AIC.Ranun.abort-min(AIC.Ranun.abort) 
best.Ranun.abort <- which(DeltaAIC.Ranun.abort==0, arr.ind = T)
colnames(SD)[best.Ranun.abort[1]]

# Achil.mille values
AIC.Achil.mille <- matrix(NA,snowdim)  
for(i in 1:snowdim){AIC.Achil.mille[i,] <- AIC(lm(P1$Achil.mille~SD[,i]))}  
DeltaAIC.Achil.mille<-AIC.Achil.mille-min(AIC.Achil.mille) 
best.Achil.mille <- which(DeltaAIC.Achil.mille==0, arr.ind = T)
colnames(SD)[best.Achil.mille[1]] 

# Zizia.aurea values
AIC.Zizia.aurea <- matrix(NA,snowdim)  
for(i in 1:snowdim){AIC.Zizia.aurea[i,] <- AIC(lm(P1$Zizia.aurea~SD[,i]))}  
DeltaAIC.Zizia.aurea<-AIC.Zizia.aurea-min(AIC.Zizia.aurea) 
best.Zizia.aurea <- which(DeltaAIC.Zizia.aurea==0, arr.ind = T)
colnames(SD)[best.Zizia.aurea[1]] 

# Rosa.arkan values
AIC.Rosa.arkan <- matrix(NA,snowdim)  
for(i in 1:snowdim){AIC.Rosa.arkan[i,] <- AIC(lm(P1$Rosa.arkan~SD[,i]))}  
DeltaAIC.Rosa.arkan<-AIC.Rosa.arkan-min(AIC.Rosa.arkan) 
best.Rosa.arkan <- which(DeltaAIC.Rosa.arkan==0, arr.ind = T)
colnames(SD)[best.Rosa.arkan[1]] 

# Vicia.ameri values
AIC.Vicia.ameri <- matrix(NA,snowdim)  
for(i in 1:snowdim){AIC.Vicia.ameri[i,] <- AIC(lm(P1$Vicia.ameri~SD[,i]))}  
DeltaAIC.Vicia.ameri<-AIC.Vicia.ameri-min(AIC.Vicia.ameri) 
best.Vicia.ameri <- which(DeltaAIC.Vicia.ameri==0, arr.ind = T)
colnames(SD)[best.Vicia.ameri[1]] 

# Litho.canes values
AIC.Litho.canes <- matrix(NA,snowdim)  
for(i in 1:snowdim){AIC.Litho.canes[i,] <- AIC(lm(P1$Litho.canes~SD[,i]))}  
DeltaAIC.Litho.canes<-AIC.Litho.canes-min(AIC.Litho.canes) 
best.Litho.canes <- which(DeltaAIC.Litho.canes==0, arr.ind = T)
colnames(SD)[best.Litho.canes[1]] 

# Amorp.canes values
AIC.Amorp.canes <- matrix(NA,snowdim)  
for(i in 1:snowdim){AIC.Amorp.canes[i,] <- AIC(lm(P1$Amorp.canes~SD[,i]))}  
DeltaAIC.Amorp.canes<-AIC.Amorp.canes-min(AIC.Amorp.canes) 
best.Amorp.canes <- which(DeltaAIC.Amorp.canes==0, arr.ind = T)
colnames(SD)[best.Amorp.canes[1]] 

# Pedic.canad values
AIC.Pedic.canad <- matrix(NA,snowdim)  
for(i in 1:snowdim){AIC.Pedic.canad[i,] <- AIC(lm(P1$Pedic.canad~SD[,i]))}  
DeltaAIC.Pedic.canad<-AIC.Pedic.canad-min(AIC.Pedic.canad) 
best.Pedic.canad <- which(DeltaAIC.Pedic.canad==0, arr.ind = T)
colnames(SD)[best.Pedic.canad[1]] 

# Trill.cernu values
AIC.Trill.cernu <- matrix(NA,snowdim)  
for(i in 1:snowdim){AIC.Trill.cernu[i,] <- AIC(lm(P1$Trill.cernu~SD[,i]))}  
DeltaAIC.Trill.cernu<-AIC.Trill.cernu-min(AIC.Trill.cernu) 
best.Trill.cernu <- which(DeltaAIC.Trill.cernu==0, arr.ind = T)
colnames(SD)[best.Trill.cernu[1]] 

# Litho.incis values
AIC.Litho.incis <- matrix(NA,snowdim)  
for(i in 1:snowdim){AIC.Litho.incis[i,] <- AIC(lm(P1$Litho.incis~SD[,i]))}  
DeltaAIC.Litho.incis<-AIC.Litho.incis-min(AIC.Litho.incis) 
best.Litho.incis <- which(DeltaAIC.Litho.incis==0, arr.ind = T)
colnames(SD)[best.Litho.incis[1]] 

# Calth.palus values
AIC.Calth.palus <- matrix(NA,snowdim)  
for(i in 1:snowdim){AIC.Calth.palus[i,] <- AIC(lm(P1$Calth.palus~SD[,i]))}  
DeltaAIC.Calth.palus<-AIC.Calth.palus-min(AIC.Calth.palus) 
best.Calth.palus <- which(DeltaAIC.Calth.palus==0, arr.ind = T)
colnames(SD)[best.Calth.palus[1]] 

# Penst.grand values
AIC.Penst.grand <- matrix(NA,snowdim)  
for(i in 1:snowdim){AIC.Penst.grand[i,] <- AIC(lm(P1$Penst.grand~SD[,i]))}  
DeltaAIC.Penst.grand<-AIC.Penst.grand-min(AIC.Penst.grand) 
best.Penst.grand <- which(DeltaAIC.Penst.grand==0, arr.ind = T)
colnames(SD)[best.Penst.grand[1]] 

# Zigad.elega values
AIC.Zigad.elega <- matrix(NA,snowdim)  
for(i in 1:snowdim){AIC.Zigad.elega[i,] <- AIC(lm(P1$Zigad.elega~SD[,i]))}  
DeltaAIC.Zigad.elega<-AIC.Zigad.elega-min(AIC.Zigad.elega) 
best.Zigad.elega <- which(DeltaAIC.Zigad.elega==0, arr.ind = T)
colnames(SD)[best.Zigad.elega[1]] 

# Campa.rotun values
AIC.Campa.rotun <- matrix(NA,snowdim)  
for(i in 1:snowdim){AIC.Campa.rotun[i,] <- AIC(lm(P1$Campa.rotun~SD[,i]))}  
DeltaAIC.Campa.rotun<-AIC.Campa.rotun-min(AIC.Campa.rotun) 
best.Campa.rotun <- which(DeltaAIC.Campa.rotun==0, arr.ind = T)
colnames(SD)[best.Campa.rotun[1]] 

# Cypri.candi values
AIC.Cypri.candi <- matrix(NA,snowdim)  
for(i in 1:snowdim){AIC.Cypri.candi[i,] <- AIC(lm(P1$Cypri.candi~SD[,i]))}  
DeltaAIC.Cypri.candi<-AIC.Cypri.candi-min(AIC.Cypri.candi) 
best.Cypri.candi <- which(DeltaAIC.Cypri.candi==0, arr.ind = T)
colnames(SD)[best.Cypri.candi[1]] 

# Zizia.aurea.1 values
AIC.Zizia.aurea.1 <- matrix(NA,snowdim)  
for(i in 1:snowdim){AIC.Zizia.aurea.1[i,] <- AIC(lm(P1$Zizia.aurea.1~SD[,i]))}  
DeltaAIC.Zizia.aurea.1<-AIC.Zizia.aurea.1-min(AIC.Zizia.aurea.1) 
best.Zizia.aurea.1 <- which(DeltaAIC.Zizia.aurea.1==0, arr.ind = T)
colnames(SD)[best.Zizia.aurea.1[1]] 

# Fraga.virgi values
AIC.Fraga.virgi <- matrix(NA,snowdim)  
for(i in 1:snowdim){AIC.Fraga.virgi[i,] <- AIC(lm(P1$Fraga.virgi~SD[,i]))}  
DeltaAIC.Fraga.virgi<-AIC.Fraga.virgi-min(AIC.Fraga.virgi) 
best.Fraga.virgi <- which(DeltaAIC.Fraga.virgi==0, arr.ind = T)
colnames(SD)[best.Fraga.virgi[1]] 

# Oxytr.lambe values
AIC.Oxytr.lambe <- matrix(NA,snowdim)  
for(i in 1:snowdim){AIC.Oxytr.lambe[i,] <- AIC(lm(P1$Oxytr.lambe~SD[,i]))}  
DeltaAIC.Oxytr.lambe<-AIC.Oxytr.lambe-min(AIC.Oxytr.lambe) 
best.Oxytr.lambe <- which(DeltaAIC.Oxytr.lambe==0, arr.ind = T)
colnames(SD)[best.Oxytr.lambe[1]] 

# Liliu.phila values
AIC.Liliu.phila <- matrix(NA,snowdim)  
for(i in 1:snowdim){AIC.Liliu.phila[i,] <- AIC(lm(P1$Liliu.phila~SD[,i]))}  
DeltaAIC.Liliu.phila<-AIC.Liliu.phila-min(AIC.Liliu.phila) 
best.Liliu.phila <- which(DeltaAIC.Liliu.phila==0, arr.ind = T)
colnames(SD)[best.Liliu.phila[1]] 

# Oenot.nutta values
AIC.Oenot.nutta <- matrix(NA,snowdim)  
for(i in 1:snowdim){AIC.Oenot.nutta[i,] <- AIC(lm(P1$Oenot.nutta~SD[,i]))}  
DeltaAIC.Oenot.nutta<-AIC.Oenot.nutta-min(AIC.Oenot.nutta) 
best.Oenot.nutta <- which(DeltaAIC.Oenot.nutta==0, arr.ind = T)
colnames(SD)[best.Oenot.nutta[1]] 


# Create spreadsheet with best day data for each species
#load list of species
bareday$day <- c(best.Oxali.viola[1], best.Ranun.rhomb[1], best.Ceras.arven[1], 
                 best.Sisyr.angus[1], best.Anemo.paten[1], best.Anemo.canad[1], 
                 best.Penst.graci[1], best.Ranun.abort[1], best.Achil.mille[1], 
                 best.Zizia.aurea[1], best.Rosa.arkan[1], best.Vicia.ameri[1], 
                 best.Litho.canes[1], best.Amorp.canes[1], best.Pedic.canad[1], 
                 best.Trill.cernu[1], best.Trill.cernu[1], best.Calth.palus[1], 
                 best.Penst.grand[1], best.Zigad.elega[1], best.Campa.rotun[1], 
                 best.Cypri.candi[1], best.Zizia.aurea.1[1], best.Fraga.virgi[1], 
                 best.Oxytr.lambe[1], best.Liliu.phila[1], best.Oenot.nutta[1])



write_xlsx(bareday, "Data/bareday_filled_col.xlsx")

