# Flowering day and snow depth analysis for single species



remove(list = ls())


SD <- read.csv( file = "Data/March_snow.csv", 
                header = T, stringsAsFactors = F)
P1 <- read.csv( file = "Data/common_species.csv",
                header = T, stringsAsFactors = F)



# create storage matrix for AIC values
snowdim <- dim(SD)[2]
AIC.Oxali.viola <- matrix(NA,snowdim) 


#loop through all 'i' possible DU sets and extract AIC values
for(i in 1:snowdim){AIC.Oxali.viola[i,] <- AIC(lm(P1$Oxali.viola~SD[,i]))}  
#calculate delta-AICs
DeltaAIC.Oxali.viola<-AIC.Oxali.viola-min(AIC.Oxali.viola) 
#best fitting model
best.Oxali.viola <- which(DeltaAIC.Oxali.viola==0, arr.ind = T)  
#which SD is it?
colnames(SD)[best.Oxali.viola[1]] 

# make scatter plot of plant FFD vs snow depth on optimal day
plot(P1$Oxali.viola~SD$X25,xlab="Snow",ylab="Oxali.viola",col="purple")


