#' # Snow depth and first flowering day correlations
#' 
remove(list = ls())


#' ### Load Libraries
library(ggplot2) 
library(gridExtra)
library(grid)
library(lattice)
library(ggpubr)
library(extrafont)

#' 
#' 
#' 
#' ### Load Data
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
ranun.arbor <- read.csv( file = "Data/ranun.arbor.csv", header = T, stringsAsFactors = F)
litho.incis <- read.csv( file = "Data/litho.incis.csv", header = T, stringsAsFactors = F)
trill.cernu <- read.csv( file = "Data/trill.cernu.csv", header = T, stringsAsFactors = F)


                                                 
#' ### Converd FFD to numeric values
achil.mille$FFD <- as.numeric(achil.mille$FFD)
amorp.canes$FFD <- as.numeric(amorp.canes$FFD)
anemo.canad$FFD <- as.numeric(anemo.canad$FFD)
anemo.paten$FFD <- as.numeric(anemo.paten$FFD)
calth.palus$FFD <- as.numeric(calth.palus$FFD)
campa.rotun$FFD <- as.numeric(campa.rotun$FFD)
ceras.arven$FFD <- as.numeric(ceras.arven$FFD)
cypri.candi$FFD <- as.numeric(cypri.candi$FFD)
litho.canes$FFD <- as.numeric(litho.canes$FFD)
oenot.nutta$FFD <- as.numeric(oenot.nutta$FFD)
oxali.viola$FFD <- as.numeric(oxali.viola$FFD)
oxytr.lambe$FFD <- as.numeric(oxytr.lambe$FFD)
pedic.canad$FFD <- as.numeric(pedic.canad$FFD)
penst.graci$FFD <- as.numeric(penst.graci$FFD)
penst.grand$FFD <- as.numeric(penst.grand$FFD)
ranun.rhomb$FFD <- as.numeric(ranun.rhomb$FFD)
rosa.arkan$FFD <- as.numeric(rosa.arkan$FFD)
sisy.angus$FFD <- as.numeric(sisy.angus$FFD)
vicia.ameri$FFD <- as.numeric(vicia.ameri$FFD)
zigad.elaga$FFD <- as.numeric(zigad.elaga$FFD)
zizia.aurea$FFD <- as.numeric(zizia.aurea$FFD)
ranun.arbor$FFD <- as.numeric(ranun.arbor$FFD)
litho.incis$FFD <- as.numeric(litho.incis$FFD)
trill.cernu$FFD <- as.numeric(trill.cernu$FFD)


#' ### Linear model for each species
lm_achil.mille <- lm(data = achil.mille, FFD ~ SPDX)
lm_amorp.canes <- lm(data = amorp.canes, FFD ~ SPDX)
lm_anemo.canad <- lm(data = anemo.canad, FFD ~ SPDX)
lm_anemo.paten <- lm(data = anemo.paten, FFD ~ SPDX)
lm_calth.palus <- lm(data = calth.palus, FFD ~ SPDX)
lm_campa.rotun <- lm(data = campa.rotun, FFD ~ SPDX)
lm_ceras.arven <- lm(data = ceras.arven, FFD ~ SPDX)
lm_cypri.candi <- lm(data = cypri.candi, FFD ~ SPDX)
lm_litho.canes <- lm(data = litho.canes, FFD ~ SPDX)
lm_oenot.nutta <- lm(data = oenot.nutta, FFD ~ SPDX)
lm_oxali.viola <- lm(data = oxali.viola, FFD ~ SPDX)
lm_oxytr.lambe <- lm(data = oxytr.lambe, FFD ~ SPDX)
lm_pedic.canad <- lm(data = pedic.canad, FFD ~ SPDX)
lm_penst.graci <- lm(data = penst.graci, FFD ~ SPDX)
lm_penst.grand <- lm(data = penst.grand, FFD ~ SPDX)
lm_ranun.rhomb <- lm(data = ranun.rhomb, FFD ~ SPDX)
lm_rosa.arkan <- lm(data = rosa.arkan, FFD ~ SPDX)
lm_sisy.angus <- lm(data = sisy.angus, FFD ~ SPDX)
lm_vicia.ameri <- lm(data = vicia.ameri, FFD ~ SPDX)
lm_zigad.elaga <- lm(data = zigad.elaga, FFD ~ SPDX)
lm_zizia.aurea <- lm(data = zizia.aurea, FFD ~ SPDX)
lm_litho.incis <- lm(data = litho.incis, FFD ~ SPDX)
lm_ranun.arbor <- lm(data = ranun.arbor, FFD ~ SPDX)
lm_trill.cernu <- lm(data = trill.cernu, FFD ~ SPDX)



#' ### ggscatter method with grid.arrange
AM <- ggscatter(achil.mille, x = "SPDX", y = "FFD", size = 1, add = "reg.line") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x.npc = "left",            
    label.y = 130) +
  theme_classic() +
  ylim(125,170)+
  ggtitle("Achillea millefolium") +
  xlab("") + ylab("") +
  theme(title = element_text(face="italic", size = 10),         
        axis.text.y=element_text(size=10, colour = "black"),         
        axis.title.y=element_text(size=2, colour = "black"),         
        axis.text.x=element_text(size = 10, colour = "black"),                  
        axis.title.x=element_text(size = 2, colour = "black"),         
        plot.margin=unit(c(0.5,0,0,0), "cm"))
# plot(AM)

AC <- ggscatter(amorp.canes, x = "SPDX", y = "FFD", size = 1, add = "reg.line") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x.npc = "left",            
    label.y = 150) + 
  theme_classic() +
  ylim(145,195)+
  ggtitle("Amorpha canescens") +
  xlab("") + ylab("") +
  theme(title = element_text(face="italic", size = 10),         
        axis.text.y=element_text(size=10, colour = "black"),         
        axis.title.y=element_text(size=2, colour = "black"),         
        axis.text.x=element_text(size = 10, colour = "black"),                  
        axis.title.x=element_text(size = 2, colour = "black"),                  
        plot.margin=unit(c(0.5,0,0,0), "cm"))
# plot(AC)

AnC <- ggscatter(anemo.canad, x = "SPDX", y = "FFD", size = 1, add = "reg.line") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x.npc = "left",            
    label.y = 130) +
  theme_classic() +
  ylim(125,170)+
  ggtitle("Anemone canadensis") +
  xlab("") + ylab("") +
  theme(title = element_text(face="italic", size = 10),         
        axis.text.y=element_text(size=10, colour = "black"),         
        axis.title.y=element_text(size=2, colour = "black"),         
        axis.text.x=element_text(size = 10, colour = "black"),                  
        axis.title.x=element_text(size = 2, colour = "black"),                  
        plot.margin=unit(c(0.5,0,0,0), "cm"))
# plot(AnC)

AP <- ggscatter(anemo.paten, x = "SPDX", y = "FFD", size = 1, add = "reg.line") + 
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x.npc = "left",            
    label.y = 75) +
  theme_classic() +
  ylim(70,160)+
  ggtitle("Anemone patens") +
  xlab("") + ylab("") +
  theme(title = element_text(face="italic", size = 10),         
        axis.text.y=element_text(size=10, colour = "black"),         
        axis.title.y=element_text(size=2, colour = "black"),         
        axis.text.x=element_text(size = 10, colour = "black"),                  
        axis.title.x=element_text(size = 2, colour = "black"),                  
        plot.margin=unit(c(0.5,0,0,0), "cm"))
# plot(AP)

CP <- ggscatter(calth.palus, x = "SPDX", y = "FFD", size = 1, add = "reg.line") + 
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x.npc = "left",            
           label.y = 100) +
  theme_classic() +
  xlim(0,15) +
  ylim(95,140)+
  ggtitle("Caltha palustris") +
  xlab("") + ylab("") +
  theme(title = element_text(face="italic", size = 10),         
        axis.text.y=element_text(size=10, colour = "black"),         
        axis.title.y=element_text(size=2, colour = "black"),         
        axis.text.x=element_text(size = 10, colour = "black"),                  
        axis.title.x=element_text(size = 2, colour = "black"),                  
        plot.margin=unit(c(0.5,0,0,0), "cm"))
# plot(CP)

CC <- ggscatter(cypri.candi, x = "SPDX", y = "FFD", size = 1, add = "reg.line") + 
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x.npc = "left",            
           label.y = 140) +
  theme_classic() +
  ylim(135,160)+
  ggtitle("Cypripedium candidum") +
  xlab("") + ylab("") +
  theme(title = element_text(face="italic", size = 10),         
        axis.text.y=element_text(size=10, colour = "black"),         
        axis.title.y=element_text(size=2, colour = "black"),         
        axis.text.x=element_text(size = 10, colour = "black"),                  
        axis.title.x=element_text(size = 2, colour = "black"),                  
        plot.margin=unit(c(0.5,0,0,0), "cm"))
# plot(CC)

CR <- ggscatter(campa.rotun, x = "SPDX", y = "FFD", size = 1, add = "reg.line") + 
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x.npc = "left",            
           label.y = 155) +
  theme_classic() +
  ylim(150,220)+
  ggtitle("Campanula rotundifolia") +
  xlab("") + ylab("") +
  theme(title = element_text(face="italic", size = 10),         
        axis.text.y=element_text(size=10, colour = "black"),         
        axis.title.y=element_text(size=2, colour = "black"),         
        axis.text.x=element_text(size = 10, colour = "black"),                  
        axis.title.x=element_text(size = 2, colour = "black"),                  
        plot.margin=unit(c(0.5,0,0,0), "cm"))
# plot(CR)

CA <- ggscatter(ceras.arven, x = "SPDX", y = "FFD", size = 1, add = "reg.line") + 
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x.npc = "left",            
           label.y = 110) +
  theme_classic() +
  ylim(105,150)+
  ggtitle("Cerastium arvense") +
  xlab("") + ylab("") +
  theme(title = element_text(face="italic", size = 10),         
        axis.text.y=element_text(size=10, colour = "black"),         
        axis.title.y=element_text(size=2, colour = "black"),         
        axis.text.x=element_text(size = 10, colour = "black"),                  
        axis.title.x=element_text(size = 2, colour = "black"),                  
        plot.margin=unit(c(0.5,0,0,0), "cm"))
# plot(CA)

LC <- ggscatter(litho.canes, x = "SPDX", y = "FFD", size = 1, add = "reg.line") + 
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x.npc = "left",            
           label.y = 110) +
  theme_classic() +
  ylim(105,150)+
  ggtitle("Lithospermum canescens") +
  xlab("") + ylab("") +
  theme(title = element_text(face="italic", size = 10),         
        axis.text.y=element_text(size=10, colour = "black"),         
        axis.title.y=element_text(size=2, colour = "black"),         
        axis.text.x=element_text(size = 10, colour = "black"),                  
        axis.title.x=element_text(size = 2, colour = "black"),                  
        plot.margin=unit(c(0.5,0,0,0), "cm"))
# plot(LC)

ON <- ggscatter(oenot.nutta, x = "SPDX", y = "FFD", size = 1, add = "reg.line") + 
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x.npc = "left",            
           label.y = 160) +
  theme_classic() +
  ylim(155,240)+
  ggtitle("Oenothera nuttallii") +
  xlab("") + ylab("") +
  theme(title = element_text(face="italic", size = 10),         
        axis.text.y=element_text(size=10, colour = "black"),         
        axis.title.y=element_text(size=2, colour = "black"),         
        axis.text.x=element_text(size = 10, colour = "black"),                  
        axis.title.x=element_text(size = 2, colour = "black"),                  
        plot.margin=unit(c(0.5,0,0,0), "cm"))
# plot(ON)

OV <- ggscatter(oxali.viola, x = "SPDX", y = "FFD", size = 1, add = "reg.line") + 
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x.npc = "left",            
           label.y = 115) +
  theme_classic() +
  ylim(110,150)+
  ggtitle("Oxalis violacea") +
  xlab("") + ylab("") +
  theme(title = element_text(face="italic", size = 10),         
        axis.text.y=element_text(size=10, colour = "black"),         
        axis.title.y=element_text(size=2, colour = "black"),         
        axis.text.x=element_text(size = 10, colour = "black"),                  
        axis.title.x=element_text(size = 2, colour = "black"),                  
        plot.margin=unit(c(0.5,0,0,0), "cm"))
# plot(OV)

OL <- ggscatter(oxytr.lambe, x = "SPDX", y = "FFD", size = 1, add = "reg.line") + 
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x.npc = "left",            
           label.y = 145) +
  theme_classic() +
  ylim(140,185)+
  ggtitle("Oxytre lambe") +
  xlab("") + ylab("") +
  theme(title = element_text(face="italic", size = 10),         
        axis.text.y=element_text(size=10, colour = "black"),         
        axis.title.y=element_text(size=2, colour = "black"),         
        axis.text.x=element_text(size = 10, colour = "black"),                  
        axis.title.x=element_text(size = 2, colour = "black"),                  
        plot.margin=unit(c(0.5,0,0,0), "cm"))
# plot(OL)

PC <- ggscatter(pedic.canad, x = "SPDX", y = "FFD", size = 1, add = "reg.line") + 
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x.npc = "left",            
           label.y = 125) +
  theme_classic() +
  xlim(0,8) +
  ylim(120,155)+
  ggtitle("Pedicularis canadensis") +
  xlab("") + ylab("") +
  theme(title = element_text(face="italic", size = 10),         
        axis.text.y=element_text(size=10, colour = "black"),         
        axis.title.y=element_text(size=2, colour = "black"),         
        axis.text.x=element_text(size = 10, colour = "black"),                  
        axis.title.x=element_text(size = 2, colour = "black"),                  
        plot.margin=unit(c(0.5,0,0,0), "cm"))
# plot(PC)

PG <- ggscatter(penst.graci, x = "SPDX", y = "FFD", size = 1, add = "reg.line") + 
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x.npc = "left",            
           label.y = 150) +
  theme_classic() +
  ylim(145,175)+
  ggtitle("Penstemon gracilis") +
  xlab("") + ylab("") +
  theme(title = element_text(face="italic", size = 10),         
        axis.text.y=element_text(size=10, colour = "black"),         
        axis.title.y=element_text(size=2, colour = "black"),         
        axis.text.x=element_text(size = 10, colour = "black"),                  
        axis.title.x=element_text(size = 2, colour = "black"),                  
        plot.margin=unit(c(0.5,0,0,0), "cm"))
# plot(PG)

PeG <- ggscatter(penst.grand, x = "SPDX", y = "FFD", size = 1, add = "reg.line") + 
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x.npc = "left",            
           label.y = 150) +
  theme_classic() +
  ylim(145,175)+
  ggtitle("Penstemon grandiflorus") +
  xlab("") + ylab("") +
  theme(title = element_text(face="italic", size = 10),         
        axis.text.y=element_text(size=10, colour = "black"),         
        axis.title.y=element_text(size=2, colour = "black"),         
        axis.text.x=element_text(size = 10, colour = "black"),                  
        axis.title.x=element_text(size = 2, colour = "black"),                  
        plot.margin=unit(c(0.5,0,0,0), "cm"))
# plot(PeG)

RR <- ggscatter(ranun.rhomb, x = "SPDX", y = "FFD", size = 1, add = "reg.line") + 
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x.npc = "left",
           label.y = 90) +
  theme_classic() +
  ylim(85,140)+
  ggtitle("Ranunculus rhomboides") +
  xlab("") + ylab("") +
  theme(title = element_text(face="italic", size = 10),         
        axis.text.y=element_text(size=10, colour = "black"),         
        axis.title.y=element_text(size=2, colour = "black"),         
        axis.text.x=element_text(size = 10, colour = "black"),                  
        axis.title.x=element_text(size = 2, colour = "black"),                  
        plot.margin=unit(c(0.5,0,0,0), "cm"))
# plot(RR)

RA <- ggscatter(rosa.arkan, x = "SPDX", y = "FFD", size = 1, add = "reg.line") + 
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x.npc = "left",            
           label.y = 145) +
  theme_classic() +
  ylim(140,180)+
  ggtitle("Rosa arkansana") +
  xlab("") + ylab("") +
  theme(title = element_text(face="italic", size = 10),         
        axis.text.y=element_text(size=10, colour = "black"),         
        axis.title.y=element_text(size=2, colour = "black"),         
        axis.text.x=element_text(size = 10, colour = "black"),                  
        axis.title.x=element_text(size = 2, colour = "black"),                  
        plot.margin=unit(c(0.5,0,0,0), "cm"))
# plot(RA)

SA <- ggscatter(sisy.angus, x = "SPDX", y = "FFD", size = 1, add = "reg.line") + 
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x.npc = "left",            
           label.y = 115) +
  theme_classic() +
  ylim(110,155)+
  ggtitle("Sisyrinchium angustifolium") +
  xlab("") + ylab("") +
  theme(title = element_text(face="italic", size = 10),         
        axis.text.y=element_text(size=10, colour = "black"),         
        axis.title.y=element_text(size=2, colour = "black"),         
        axis.text.x=element_text(size = 10, colour = "black"),                  
        axis.title.x=element_text(size = 2, colour = "black"),                  
        plot.margin=unit(c(0.5,0,0,0), "cm"))
# plot(SA)

VA <- ggscatter(vicia.ameri, x = "SPDX", y = "FFD", size = 1, add = "reg.line") + 
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x.npc = "left",            
           label.y = 130) +
  theme_classic() +
  ylim(125,170)+
  ggtitle("Vicia americana") +
  xlab("") + ylab("") +
  theme(title = element_text(face="italic", size = 10),         
        axis.text.y=element_text(size=10, colour = "black"),         
        axis.title.y=element_text(size=2, colour = "black"),         
        axis.text.x=element_text(size = 10, colour = "black"),                  
        axis.title.x=element_text(size = 2, colour = "black"),                  
        plot.margin=unit(c(0.5,0,0,0), "cm"))
# plot(VA)


ZE <- ggscatter(zigad.elaga, x = "SPDX", y = "FFD", size = 1, add = "reg.line") + 
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x.npc = "left",            
           label.y = 155) +
  theme_classic() +
  ylim(150,190)+
  ggtitle("Zigadenus elegans") +
  xlab("") + ylab("") +
  theme(title = element_text(face="italic", size = 10),
        axis.text.y=element_text(size=10, colour = "black"),
        axis.title.y=element_text(size=2, colour = "black"),
        axis.text.x=element_text(size = 10, colour = "black"),         
        axis.title.x=element_text(size = 2, colour = "black"),                  
        plot.margin=unit(c(0.5,0,0,0), "cm"))
# plot(ZE)

ZA <- ggscatter(zizia.aurea, x = "SPDX", y = "FFD", size = 1, add = "reg.line") + 
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x.npc = "left",            
           label.y = 120) +
  theme_classic() +
  xlim(0,8) +
  ylim(115,170)+
  ggtitle("Zizia aurea") +
  xlab("") + ylab("") +
  theme(title = element_text(face="italic", size = 10),         
        axis.text.y=element_text(size=10, colour = "black"),         
        axis.title.y=element_text(size=2, colour = "black"),         
        axis.text.x=element_text(size = 10, colour = "black"),         
        axis.title.x=element_text(size = 2, colour = "black"),         
        plot.margin=unit(c(0.5,0,0,0), "cm"))
# plot(ZA)

LI <- ggscatter(litho.incis, x = "SPDX", y = "FFD", size = 1, add = "reg.line") + 
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x.npc = "left",            
           label.y = 120) +
  theme_classic() +
  # xlim(0,8) +
  # ylim(115,170)+
  ggtitle("Lithospermum incisum") +
  xlab("") + ylab("") +
  theme(title = element_text(face="italic", size = 10),         
        axis.text.y=element_text(size=10, colour = "black"),         
        axis.title.y=element_text(size=2, colour = "black"),         
        axis.text.x=element_text(size = 10, colour = "black"),         
        axis.title.x=element_text(size = 2, colour = "black"),         
        plot.margin=unit(c(0.5,0,0,0), "cm"))
# plot(LI)

RaA <- ggscatter(ranun.rhomb, x = "SPDX", y = "FFD", size = 1, add = "reg.line") + 
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x.npc = "left",            
           label.y = 120) +
  theme_classic() +
  # xlim(0,8) +
  # ylim(115,170)+
  ggtitle("Ranunculus abortivus") +
  xlab("") + ylab("") +
  theme(title = element_text(face="italic", size = 10),         
        axis.text.y=element_text(size=10, colour = "black"),         
        axis.title.y=element_text(size=2, colour = "black"),         
        axis.text.x=element_text(size = 10, colour = "black"),         
        axis.title.x=element_text(size = 2, colour = "black"),         
        plot.margin=unit(c(0.5,0,0,0), "cm"))
# plot(RaA)

TC <- ggscatter(trill.cernu, x = "SPDX", y = "FFD", size = 1, add = "reg.line") + 
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x.npc = "left",            
           label.y = 160) +
  theme_classic() +
  xlim(0,18) +
  # ylim(155,170)+
  ggtitle("Trillium cernuum") +
  xlab("") + ylab("") +
  theme(title = element_text(face="italic", size = 10),         
        axis.text.y=element_text(size=10, colour = "black"),         
        axis.title.y=element_text(size=2, colour = "black"),         
        axis.text.x=element_text(size = 10, colour = "black"),         
        axis.title.x=element_text(size = 2, colour = "black"),         
        plot.margin=unit(c(0.5,0,0,0), "cm"))
# plot(TC)

SPDX_FFD_grid <- grid.arrange(arrangeGrob(AP, RR, CP, CA, RaA, OV, 
                                          SA, LC, TC, LI,
                                          PC, ZA, VA, CC, AM, AnC, OL, 
                                          RA, PeG, PG, CR, 
                                          ZE, AC, ON, 
                         ncol=4, nrow=6, 
                         bottom = textGrob("SPDX", vjust = 1),
                         left = textGrob("FFD", rot = 90, vjust = 1), vp=viewport(width=0.95, height=0.95)))
grid.draw(SPDX_FFD_grid)

pdf("Figures/SPDX_FFD_grid.pdf",width = 8.5, height = 11) 
grid.draw(SPDX_FFD_grid) 
dev.off()

png(filename = "Figures/SPDX_FFD_grid.png", width = 563, height = 800, units = "px")
grid.draw(SPDX_FFD_grid) 
dev.off()


#' 
#' #' ### ggplot method with grid.arrange
#' AM <- ggplot(achil.mille, aes(x = SPDX, y = FFD)) +
#'   geom_point() +
#'   geom_smooth(method = "lm", col = "black") +
#'   theme_classic() +
#'   ggtitle("Achillea millefolium") +
#'   xlab("") + ylab("") +
#'   scale_x_continuous(limits = c(10,40), expand = c(0,0))
#' plot(AM)
#' 
#' 
#' 
#' #' ## Linear model and individual plots for each species using base R
#' # jpeg(filename = "Figures/SPDX_FFDcorrelations.jpeg")
#' par()
#' par(mfrow=c(7,3), mar=c(2,2,2,2), oma = c(1,1,1,1))
#' 
#' plot(achil.mille$SPDX, achil.mille$FFD,
#'      main = "Achillea millefolium",
#'      xlab = "",
#'      ylab = "")
#' abline(lm_achil.mille)
#' # summary(lm_achil.mille)
#' 
#' plot(amorp.canes$SPDX, amorp.canes$FFD,
#'      main = "Amorpha canescens",
#'      xlab = "",
#'      ylab = "")
#' abline(lm_amorp.canes)
#' # summary(lm_amorp.canes)
#' 
#' plot(anemo.canad$SPDX, anemo.canad$FFD,
#'      main = "Anemone canadensis",
#'      xlab = "",
#'      ylab = "")
#' abline(lm_anemo.canad)
#' # summary(lm_anemo.canad)
#' 
#' plot(anemo.paten$SPDX, anemo.paten$FFD,
#'      main = "Anemone patens",
#'      xlab = "",
#'      ylab = "")
#' abline(lm_anemo.paten)
#' # summary(lm_anemo.paten)
#' 
#' plot(calth.palus$SPDX, calth.palus$FFD,
#'      main = "Caltha palustris",
#'      xlab = "",
#'      ylab = "")
#' abline(lm_calth.palus)
#' # summary(lm_calth.palus)
#' 
#' plot(campa.rotun$SPDX, campa.rotun$FFD,
#'      main = "Campanula rotundifolia",
#'      xlab = "",
#'      ylab = "")
#' abline(lm_campa.rotun)
#' # summary(lm_campa.rotun)
#' 
#' plot(ceras.arven$SPDX, ceras.arven$FFD,
#'      main = "Cerastium arvense",
#'      xlab = "",
#'      ylab = "")
#' abline(lm_ceras.arven)
#' # summary(lm_ceras.arven)
#' 
#' plot(cypri.candi$SPDX, cypri.candi$FFD,
#'      main = "Cypripedium candidum",
#'      xlab = "",
#'      ylab = "")
#' abline(lm_cypri.candi)
#' # summary(lm_cypri.candi)
#' 
#' plot(litho.canes$SPDX, litho.canes$FFD,
#'      main = "Lithospermum canescens",
#'      xlab = "",
#'      ylab = "")
#' abline(lm_litho.canes)
#' # summary(lm_litho.canes)
#' 
#' plot(oenot.nutta$SPDX, oenot.nutta$FFD,
#'      main = "Oenothera nuttallii",
#'      xlab = "",
#'      ylab = "")
#' abline(lm_oenot.nutta)
#' # summary(lm_oenot.nutta)
#' 
#' plot(oxali.viola$SPDX, oxali.viola$FFD,
#'      main = "Oxalis violacea",
#'      xlab = "",
#'      ylab = "")
#' abline(lm_oxali.viola)
#' # summary(lm_oxali.viola)
#' 
#' plot(oxytr.lambe$SPDX, oxytr.lambe$FFD,
#'      main = "Oxytre lambe",
#'      xlab = "",
#'      ylab = "")
#' abline(lm_oxytr.lambe)
#' # summary(lm_oxytr.lambe)
#' 
#' plot(pedic.canad$SPDX, pedic.canad$FFD,
#'      main = "Pedicularis canadensis",
#'      xlab = "",
#'      ylab = "")
#' abline(lm_pedic.canad)
#' # summary(lm_pedic.canad)
#' 
#' plot(penst.graci$SPDX, penst.graci$FFD,
#'      main = "Penstemon gracilis",
#'      xlab = "",
#'      ylab = "")
#' abline(lm_penst.graci)
#' # summary(lm_penst.graci)
#' 
#' plot(penst.grand$SPDX, penst.grand$FFD,
#'      main = "Penstemon grandiflorus",
#'      xlab = "",
#'      ylab = "")
#' abline(lm_penst.grand)
#' # summary(lm_penst.grand)
#' 
#' plot(ranun.rhomb$SPDX, ranun.rhomb$FFD,
#'      main = "Ranunculus rhomboides",
#'      xlab = "",
#'      ylab = "")
#' abline(lm_ranun.rhomb)
#' # summary(lm_ranun.rhomb)
#' 
#' plot(rosa.arkan$SPDX, rosa.arkan$FFD,
#'      main = "Rosa arkansana",
#'      xlab = "",
#'      ylab = "")
#' abline(lm_rosa.arkan)
#' # summary(lm_rosa.arkan)
#' 
#' plot(sisy.angus$SPDX, sisy.angus$FFD,
#'      main = "Sisyrinchium angustifolium",
#'      xlab = "",
#'      ylab = "")
#' abline(lm_sisy.angus)
#' # summary(lm_sisy.angus)
#' 
#' plot(vicia.ameri$SPDX, vicia.ameri$FFD,
#'      main = "Vicia americana",
#'      xlab = "",
#'      ylab = "")
#' abline(lm_vicia.ameri)
#' # summary(lm_vicia.ameri)
#' 
#' plot(zigad.elaga$SPDX, zigad.elaga$FFD,
#'      main = "Zigadenus elegans",
#'      xlab = "",
#'      ylab = "")
#' abline(lm_zigad.elaga)
#' # summary(lm_zigad.elaga)
#' 
#' plot(zizia.aurea$SPDX, zizia.aurea$FFD,
#'      main = "Zizia aurea",
#'      xlab = "",
#'      ylab = "")
#' abline(lm_zizia.aurea)
#' # summary(lm_zizia.aurea)
#' # dev.off()
#' 
#' 
