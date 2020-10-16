#' # Snow depth and first flowering day correlations
#' 
remove(list = ls())


#' ### Load Libraries
library(ggplot2) 
library(gridExtra)
library(grid)
library(lattice)

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


#' ## Linear model and individual plots for each species
# jpeg(filename = "Figures/TSNOW_FFDcorrelations.jpeg")
par(mfrow=c(7,3))
par(mar=c(1,1,1,1))

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




#' ### ggplot method with grid.arrange
AM <- ggplot(achil.mille, aes(x = TSNOW, y = FFD)) +
  geom_point() +
  geom_smooth(method = "lm", col = "black") +
  theme_bw() +
  ggtitle("Achillea millefolium") +
  xlab("") + ylab("") +
  scale_x_continuous(limits = c(10,40), expand = c(0,0))
plot(AM)

AC <- ggplot(amorp.canes, aes(x = TSNOW, y = FFD)) +
  geom_point() +
  geom_smooth(method = "lm", col = "black") +
  theme_bw() +
  ggtitle("Amorpha canescens") +
  xlab("") + ylab("") +
  scale_x_continuous(limits = c(10,40), expand = c(0,0))
plot(AC)

AnC <- ggplot(anemo.canad, aes(x = TSNOW, y = FFD)) +
  geom_point() +
  geom_smooth(method = "lm", col = "black") +
  theme_bw() +
  ggtitle("Anemone canadensis") +
  xlab("") + ylab("") +
  scale_x_continuous(limits = c(10,40), expand = c(0,0))
plot(AnC)

AP <- ggplot(anemo.paten, aes(x = TSNOW, y = FFD)) +
  geom_point() +
  geom_smooth(method = "lm", col = "black") +
  theme_bw() +
  ggtitle("Anemone patens") +
  xlab("") + ylab("") +
  scale_x_continuous(limits = c(10,40), expand = c(0,0))
plot(AP)


grid.arrange(arrangeGrob(AM, AC, AnC,
                         ncol=3, nrow=1), heights = 1)


#grid.arrange(arrangeGrob(AM, AC, AnC, AP, CP, CR, CA, CC, LC, ON, OV, OL, 
#                          PC, PG, PeG, RR, RA, SA, VA, ZE, ZA, 
#                          ncol=3, nrow=7), heights = 1)
# 
# 







