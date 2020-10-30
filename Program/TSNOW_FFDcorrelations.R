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


#' ### Linear model for each species
lm_achil.mille <- lm(data = achil.mille, FFD ~ TSNOW)
lm_amorp.canes <- lm(data = amorp.canes, FFD ~ TSNOW)
lm_anemo.canad <- lm(data = anemo.canad, FFD ~ TSNOW)
lm_anemo.paten <- lm(data = anemo.paten, FFD ~ TSNOW)
lm_calth.palus <- lm(data = calth.palus, FFD ~ TSNOW)
lm_campa.rotun <- lm(data = campa.rotun, FFD ~ TSNOW)
lm_ceras.arven <- lm(data = ceras.arven, FFD ~ TSNOW)
lm_cypri.candi <- lm(data = cypri.candi, FFD ~ TSNOW)
lm_litho.canes <- lm(data = litho.canes, FFD ~ TSNOW)
lm_oenot.nutta <- lm(data = oenot.nutta, FFD ~ TSNOW)
lm_oxali.viola <- lm(data = oxali.viola, FFD ~ TSNOW)
lm_oxytr.lambe <- lm(data = oxytr.lambe, FFD ~ TSNOW)
lm_pedic.canad <- lm(data = pedic.canad, FFD ~ TSNOW)
lm_penst.graci <- lm(data = penst.graci, FFD ~ TSNOW)
lm_penst.grand <- lm(data = penst.grand, FFD ~ TSNOW)
lm_ranun.rhomb <- lm(data = ranun.rhomb, FFD ~ TSNOW)
lm_rosa.arkan <- lm(data = rosa.arkan, FFD ~ TSNOW)
lm_sisy.angus <- lm(data = sisy.angus, FFD ~ TSNOW)
lm_vicia.ameri <- lm(data = vicia.ameri, FFD ~ TSNOW)
lm_zigad.elaga <- lm(data = zigad.elaga, FFD ~ TSNOW)
lm_zizia.aurea <- lm(data = zizia.aurea, FFD ~ TSNOW)



#' ### ggscatter method with grid.arrange
AM <- ggscatter(achil.mille, x = "TSNOW", y = "FFD", size = 1, add = "reg.line") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x.npc = "left",            
    label.y = 130) +
  theme_classic() +
  ylim(125,170)+
  ggtitle("Achillea millefolium") +
  xlab("") + ylab("") +
  theme(title = element_text(face="italic"),         
        axis.text.y=element_text(size=10, colour = "black"),         
        axis.title.y=element_text(size=2, colour = "black"),         
        axis.text.x=element_text(size = 10, colour = "black"),                  
        axis.title.x=element_text(size = 2, colour = "black"),         
        plot.margin=unit(c(0.5,0,0,0), "cm"))
# plot(AM)

AC <- ggscatter(amorp.canes, x = "TSNOW", y = "FFD", size = 1, add = "reg.line") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x.npc = "left",            
    label.y = 150) + 
  theme_classic() +
  ylim(145,195)+
  ggtitle("Amorpha canescens") +
  xlab("") + ylab("") +
  theme(title = element_text(face="italic"),         
        axis.text.y=element_text(size=10, colour = "black"),         
        axis.title.y=element_text(size=2, colour = "black"),         
        axis.text.x=element_text(size = 10, colour = "black"),                  
        axis.title.x=element_text(size = 2, colour = "black"),                  
        plot.margin=unit(c(0.5,0,0,0), "cm"))
# plot(AC)

AnC <- ggscatter(anemo.canad, x = "TSNOW", y = "FFD", size = 1, add = "reg.line") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x.npc = "left",            
    label.y = 130) +
  theme_classic() +
  ylim(125,170)+
  ggtitle("Anemone canadensis") +
  xlab("") + ylab("") +
  theme(title = element_text(face="italic"),         
        axis.text.y=element_text(size=10, colour = "black"),         
        axis.title.y=element_text(size=2, colour = "black"),         
        axis.text.x=element_text(size = 10, colour = "black"),                  
        axis.title.x=element_text(size = 2, colour = "black"),                  
        plot.margin=unit(c(0.5,0,0,0), "cm"))
# plot(AnC)

AP <- ggscatter(anemo.paten, x = "TSNOW", y = "FFD", size = 1, add = "reg.line") + 
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x.npc = "left",            
    label.y = 75) +
  theme_classic() +
  ylim(70,160)+
  ggtitle("Anemone patens") +
  xlab("") + ylab("") +
  theme(title = element_text(face="italic"),         
        axis.text.y=element_text(size=10, colour = "black"),         
        axis.title.y=element_text(size=2, colour = "black"),         
        axis.text.x=element_text(size = 10, colour = "black"),                  
        axis.title.x=element_text(size = 2, colour = "black"),                  
        plot.margin=unit(c(0.5,0,0,0), "cm"))
# plot(AP)

CP <- ggscatter(calth.palus, x = "TSNOW", y = "FFD", size = 1, add = "reg.line") + 
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x.npc = "left",            
           label.y = 100) +
  theme_classic() +
  ylim(95,140)+
  ggtitle("Caltha palustris") +
  xlab("") + ylab("") +
  theme(title = element_text(face="italic"),         
        axis.text.y=element_text(size=10, colour = "black"),         
        axis.title.y=element_text(size=2, colour = "black"),         
        axis.text.x=element_text(size = 10, colour = "black"),                  
        axis.title.x=element_text(size = 2, colour = "black"),                  
        plot.margin=unit(c(0.5,0,0,0), "cm"))
# plot(CP)

CC <- ggscatter(cypri.candi, x = "TSNOW", y = "FFD", size = 1, add = "reg.line") + 
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x.npc = "left",            
           label.y = 140) +
  theme_classic() +
  ylim(135,160)+
  ggtitle("Cypripedium candidum") +
  xlab("") + ylab("") +
  theme(title = element_text(face="italic"),         
        axis.text.y=element_text(size=10, colour = "black"),         
        axis.title.y=element_text(size=2, colour = "black"),         
        axis.text.x=element_text(size = 10, colour = "black"),                  
        axis.title.x=element_text(size = 2, colour = "black"),                  
        plot.margin=unit(c(0.5,0,0,0), "cm"))
# plot(CC)

CR <- ggscatter(campa.rotun, x = "TSNOW", y = "FFD", size = 1, add = "reg.line") + 
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x.npc = "left",            
           label.y = 155) +
  theme_classic() +
  ylim(150,220)+
  ggtitle("Campanula rotundifolia") +
  xlab("") + ylab("") +
  theme(title = element_text(face="italic"),         
        axis.text.y=element_text(size=10, colour = "black"),         
        axis.title.y=element_text(size=2, colour = "black"),         
        axis.text.x=element_text(size = 10, colour = "black"),                  
        axis.title.x=element_text(size = 2, colour = "black"),                  
        plot.margin=unit(c(0.5,0,0,0), "cm"))
# plot(CR)

CA <- ggscatter(ceras.arven, x = "TSNOW", y = "FFD", size = 1, add = "reg.line") + 
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x.npc = "left",            
           label.y = 110) +
  theme_classic() +
  ylim(105,150)+
  ggtitle("Cerastium arvense") +
  xlab("") + ylab("") +
  theme(title = element_text(face="italic"),         
        axis.text.y=element_text(size=10, colour = "black"),         
        axis.title.y=element_text(size=2, colour = "black"),         
        axis.text.x=element_text(size = 10, colour = "black"),                  
        axis.title.x=element_text(size = 2, colour = "black"),                  
        plot.margin=unit(c(0.5,0,0,0), "cm"))
# plot(CA)

LC <- ggscatter(litho.canes, x = "TSNOW", y = "FFD", size = 1, add = "reg.line") + 
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x.npc = "left",            
           label.y = 110) +
  theme_classic() +
  ylim(105,150)+
  ggtitle("Lithospermum canescens") +
  xlab("") + ylab("") +
  theme(title = element_text(face="italic"),         
        axis.text.y=element_text(size=10, colour = "black"),         
        axis.title.y=element_text(size=2, colour = "black"),         
        axis.text.x=element_text(size = 10, colour = "black"),                  
        axis.title.x=element_text(size = 2, colour = "black"),                  
        plot.margin=unit(c(0.5,0,0,0), "cm"))
# plot(LC)

ON <- ggscatter(oenot.nutta, x = "TSNOW", y = "FFD", size = 1, add = "reg.line") + 
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x.npc = "left",            
           label.y = 160) +
  theme_classic() +
  ylim(155,240)+
  ggtitle("Oenothera nuttallii") +
  xlab("") + ylab("") +
  theme(title = element_text(face="italic"),         
        axis.text.y=element_text(size=10, colour = "black"),         
        axis.title.y=element_text(size=2, colour = "black"),         
        axis.text.x=element_text(size = 10, colour = "black"),                  
        axis.title.x=element_text(size = 2, colour = "black"),                  
        plot.margin=unit(c(0.5,0,0,0), "cm"))
# plot(ON)

OV <- ggscatter(oxali.viola, x = "TSNOW", y = "FFD", size = 1, add = "reg.line") + 
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x.npc = "left",            
           label.y = 115) +
  theme_classic() +
  ylim(110,150)+
  ggtitle("Oxalis violacea") +
  xlab("") + ylab("") +
  theme(title = element_text(face="italic"),         
        axis.text.y=element_text(size=10, colour = "black"),         
        axis.title.y=element_text(size=2, colour = "black"),         
        axis.text.x=element_text(size = 10, colour = "black"),                  
        axis.title.x=element_text(size = 2, colour = "black"),                  
        plot.margin=unit(c(0.5,0,0,0), "cm"))
# plot(OV)

OL <- ggscatter(oxytr.lambe, x = "TSNOW", y = "FFD", size = 1, add = "reg.line") + 
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x.npc = "left",            
           label.y = 145) +
  theme_classic() +
  ylim(140,185)+
  ggtitle("Oxytre lambe") +
  xlab("") + ylab("") +
  theme(title = element_text(face="italic"),         
        axis.text.y=element_text(size=10, colour = "black"),         
        axis.title.y=element_text(size=2, colour = "black"),         
        axis.text.x=element_text(size = 10, colour = "black"),                  
        axis.title.x=element_text(size = 2, colour = "black"),                  
        plot.margin=unit(c(0.5,0,0,0), "cm"))
# plot(OL)

PC <- ggscatter(pedic.canad, x = "TSNOW", y = "FFD", size = 1, add = "reg.line") + 
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x.npc = "left",            
           label.y = 125) +
  theme_classic() +
  ylim(120,155)+
  ggtitle("Pedicularis canadensis") +
  xlab("") + ylab("") +
  theme(title = element_text(face="italic"),         
        axis.text.y=element_text(size=10, colour = "black"),         
        axis.title.y=element_text(size=2, colour = "black"),         
        axis.text.x=element_text(size = 10, colour = "black"),                  
        axis.title.x=element_text(size = 2, colour = "black"),                  
        plot.margin=unit(c(0.5,0,0,0), "cm"))
# plot(PC)

PG <- ggscatter(penst.graci, x = "TSNOW", y = "FFD", size = 1, add = "reg.line") + 
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x.npc = "left",            
           label.y = 150) +
  theme_classic() +
  ylim(145,175)+
  ggtitle("Penstemon gracilis") +
  xlab("") + ylab("") +
  theme(title = element_text(face="italic"),         
        axis.text.y=element_text(size=10, colour = "black"),         
        axis.title.y=element_text(size=2, colour = "black"),         
        axis.text.x=element_text(size = 10, colour = "black"),                  
        axis.title.x=element_text(size = 2, colour = "black"),                  
        plot.margin=unit(c(0.5,0,0,0), "cm"))
# plot(PG)

PeG <- ggscatter(penst.grand, x = "TSNOW", y = "FFD", size = 1, add = "reg.line") + 
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x.npc = "left",            
           label.y = 150) +
  theme_classic() +
  ylim(145,175)+
  ggtitle("Penstemon grandiflorus") +
  xlab("") + ylab("") +
  theme(title = element_text(face="italic"),         
        axis.text.y=element_text(size=10, colour = "black"),         
        axis.title.y=element_text(size=2, colour = "black"),         
        axis.text.x=element_text(size = 10, colour = "black"),                  
        axis.title.x=element_text(size = 2, colour = "black"),                  
        plot.margin=unit(c(0.5,0,0,0), "cm"))
# plot(PeG)

RR <- ggscatter(ranun.rhomb, x = "TSNOW", y = "FFD", size = 1, add = "reg.line") + 
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x.npc = "left",
           label.y = 90) +
  theme_classic() +
  ylim(85,140)+
  ggtitle("Ranunculus rhomboides") +
  xlab("") + ylab("") +
  theme(title = element_text(face="italic"),         
        axis.text.y=element_text(size=10, colour = "black"),         
        axis.title.y=element_text(size=2, colour = "black"),         
        axis.text.x=element_text(size = 10, colour = "black"),                  
        axis.title.x=element_text(size = 2, colour = "black"),                  
        plot.margin=unit(c(0.5,0,0,0), "cm"))
# plot(RR)

RA <- ggscatter(rosa.arkan, x = "TSNOW", y = "FFD", size = 1, add = "reg.line") + 
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x.npc = "left",            
           label.y = 145) +
  theme_classic() +
  ylim(140,180)+
  ggtitle("Rosa arkansana") +
  xlab("") + ylab("") +
  theme(title = element_text(face="italic"),         
        axis.text.y=element_text(size=10, colour = "black"),         
        axis.title.y=element_text(size=2, colour = "black"),         
        axis.text.x=element_text(size = 10, colour = "black"),                  
        axis.title.x=element_text(size = 2, colour = "black"),                  
        plot.margin=unit(c(0.5,0,0,0), "cm"))
# plot(RA)

SA <- ggscatter(sisy.angus, x = "TSNOW", y = "FFD", size = 1, add = "reg.line") + 
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x.npc = "left",            
           label.y = 115) +
  theme_classic() +
  ylim(110,155)+
  ggtitle("Sisyrinchium angustifolium") +
  xlab("") + ylab("") +
  theme(title = element_text(face="italic"),         
        axis.text.y=element_text(size=10, colour = "black"),         
        axis.title.y=element_text(size=2, colour = "black"),         
        axis.text.x=element_text(size = 10, colour = "black"),                  
        axis.title.x=element_text(size = 2, colour = "black"),                  
        plot.margin=unit(c(0.5,0,0,0), "cm"))
# plot(SA)

VA <- ggscatter(vicia.ameri, x = "TSNOW", y = "FFD", size = 1, add = "reg.line") + 
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x.npc = "left",            
           label.y = 130) +
  theme_classic() +
  ylim(125,170)+
  ggtitle("Vicia americana") +
  xlab("") + ylab("") +
  theme(title = element_text(face="italic"),         
        axis.text.y=element_text(size=10, colour = "black"),         
        axis.title.y=element_text(size=2, colour = "black"),         
        axis.text.x=element_text(size = 10, colour = "black"),                  
        axis.title.x=element_text(size = 2, colour = "black"),                  
        plot.margin=unit(c(0.5,0,0,0), "cm"))
# plot(VA)


ZE <- ggscatter(zigad.elaga, x = "TSNOW", y = "FFD", size = 1, add = "reg.line") + 
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x.npc = "left",            
           label.y = 155) +
  theme_classic() +
  ylim(150,190)+
  ggtitle("Zigadenus elegans") +
  xlab("") + ylab("") +
  theme(title = element_text(face="italic"),
        axis.text.y=element_text(size=10, colour = "black"),
        axis.title.y=element_text(size=2, colour = "black"),
        axis.text.x=element_text(size = 10, colour = "black"),         
        axis.title.x=element_text(size = 2, colour = "black"),                  
        plot.margin=unit(c(0.5,0,0,0), "cm"))
# plot(ZE)

ZA <- ggscatter(zizia.aurea, x = "TSNOW", y = "FFD", size = 1, add = "reg.line") + 
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x.npc = "left",            
           label.y = 120) +
  theme_classic() +
  xlim(0,30) +
  ylim(115,170)+
  ggtitle("Zizia aurea") +
  xlab("") + ylab("") +
  theme(title = element_text(face="italic"),         
        axis.text.y=element_text(size=10, colour = "black"),         
        axis.title.y=element_text(size=2, colour = "black"),         
        axis.text.x=element_text(size = 10, colour = "black"),         
        axis.title.x=element_text(size = 2, colour = "black"),         
        plot.margin=unit(c(0.5,0,0,0), "cm"))
# plot(ZA)



TSNOW_FFD_grid <- grid.arrange(arrangeGrob(AP, CP, OV, RR, AnC, CA, CC, LC,
                                           OL, PC, PG, PeG, VA, ZA, SA, AM, 
                                           AC, CR, ON, RA, ZE, 
                         ncol=3, nrow=7, 
                         bottom = textGrob("TSNOW", vjust = 1),
                         left = textGrob("FFD", rot = 90, vjust = 1)))
pdf("Figures/TSNOW_FFD_grid.pdf",width = 8.5, height = 11) 
grid.draw(TSNOW_FFD_grid) 
dev.off()





#' ### ggplot method with grid.arrange
AM <- ggplot(achil.mille, aes(x = TSNOW, y = FFD)) +
  geom_point() +
  geom_smooth(method = "lm", col = "black") +
  theme_classic() +
  ggtitle("Achillea millefolium") +
  xlab("") + ylab("") +
  scale_x_continuous(limits = c(10,40), expand = c(0,0))
plot(AM)



#' ## Linear model and individual plots for each species using base R
# jpeg(filename = "Figures/TSNOW_FFDcorrelations.jpeg")
par()
par(mfrow=c(7,3), mar=c(2,2,2,2), oma = c(1,1,1,1))

plot(achil.mille$TSNOW, achil.mille$FFD,
     main = "Achillea millefolium",
     xlab = "",
     ylab = "")
abline(lm_achil.mille)
# summary(lm_achil.mille)

plot(amorp.canes$TSNOW, amorp.canes$FFD,
     main = "Amorpha canescens",
     xlab = "",
     ylab = "")
abline(lm_amorp.canes)
# summary(lm_amorp.canes)

plot(anemo.canad$TSNOW, anemo.canad$FFD,
     main = "Anemone canadensis",
     xlab = "",
     ylab = "")
abline(lm_anemo.canad)
# summary(lm_anemo.canad)

plot(anemo.paten$TSNOW, anemo.paten$FFD,
     main = "Anemone patens",
     xlab = "",
     ylab = "")
abline(lm_anemo.paten)
# summary(lm_anemo.paten)

plot(calth.palus$TSNOW, calth.palus$FFD,
     main = "Caltha palustris",
     xlab = "",
     ylab = "")
abline(lm_calth.palus)
# summary(lm_calth.palus)

plot(campa.rotun$TSNOW, campa.rotun$FFD,
     main = "Campanula rotundifolia",
     xlab = "",
     ylab = "")
abline(lm_campa.rotun)
# summary(lm_campa.rotun)

plot(ceras.arven$TSNOW, ceras.arven$FFD,
     main = "Cerastium arvense",
     xlab = "",
     ylab = "")
abline(lm_ceras.arven)
# summary(lm_ceras.arven)

plot(cypri.candi$TSNOW, cypri.candi$FFD,
     main = "Cypripedium candidum",
     xlab = "",
     ylab = "")
abline(lm_cypri.candi)
# summary(lm_cypri.candi)

plot(litho.canes$TSNOW, litho.canes$FFD,
     main = "Lithospermum canescens",
     xlab = "",
     ylab = "")
abline(lm_litho.canes)
# summary(lm_litho.canes)

plot(oenot.nutta$TSNOW, oenot.nutta$FFD,
     main = "Oenothera nuttallii",
     xlab = "",
     ylab = "")
abline(lm_oenot.nutta)
# summary(lm_oenot.nutta)

plot(oxali.viola$TSNOW, oxali.viola$FFD,
     main = "Oxalis violacea",
     xlab = "",
     ylab = "")
abline(lm_oxali.viola)
# summary(lm_oxali.viola)

plot(oxytr.lambe$TSNOW, oxytr.lambe$FFD,
     main = "Oxytre lambe",
     xlab = "",
     ylab = "")
abline(lm_oxytr.lambe)
# summary(lm_oxytr.lambe)

plot(pedic.canad$TSNOW, pedic.canad$FFD,
     main = "Pedicularis canadensis",
     xlab = "",
     ylab = "")
abline(lm_pedic.canad)
# summary(lm_pedic.canad)

plot(penst.graci$TSNOW, penst.graci$FFD,
     main = "Penstemon gracilis",
     xlab = "",
     ylab = "")
abline(lm_penst.graci)
# summary(lm_penst.graci)

plot(penst.grand$TSNOW, penst.grand$FFD,
     main = "Penstemon grandiflorus",
     xlab = "",
     ylab = "")
abline(lm_penst.grand)
# summary(lm_penst.grand)

plot(ranun.rhomb$TSNOW, ranun.rhomb$FFD,
     main = "Ranunculus rhomboides",
     xlab = "",
     ylab = "")
abline(lm_ranun.rhomb)
# summary(lm_ranun.rhomb)

plot(rosa.arkan$TSNOW, rosa.arkan$FFD,
     main = "Rosa arkansana",
     xlab = "",
     ylab = "")
abline(lm_rosa.arkan)
# summary(lm_rosa.arkan)

plot(sisy.angus$TSNOW, sisy.angus$FFD,
     main = "Sisyrinchium angustifolium",
     xlab = "",
     ylab = "")
abline(lm_sisy.angus)
# summary(lm_sisy.angus)

plot(vicia.ameri$TSNOW, vicia.ameri$FFD,
     main = "Vicia americana",
     xlab = "",
     ylab = "")
abline(lm_vicia.ameri)
# summary(lm_vicia.ameri)

plot(zigad.elaga$TSNOW, zigad.elaga$FFD,
     main = "Zigadenus elegans",
     xlab = "",
     ylab = "")
abline(lm_zigad.elaga)
# summary(lm_zigad.elaga)

plot(zizia.aurea$TSNOW, zizia.aurea$FFD,
     main = "Zizia aurea",
     xlab = "",
     ylab = "")
abline(lm_zizia.aurea)
# summary(lm_zizia.aurea)
# dev.off()


