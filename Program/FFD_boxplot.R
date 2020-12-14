#' First flowering data box plots for all species

library(ggplot2)


ffd4 <- read.csv(file="Data/FFD_boxplot.csv",header=T, stringsAsFactors = F)
# make boxplots

z <- ggplot(ffd4, aes(ID, FFD))+
  geom_boxplot()+
  theme_classic() +
  ylab("FFD")+ 
  xlab("Species") +
  theme(text = element_text(family = "serif"),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=14),
        axis.text.x=element_text(size = 12),
        axis.title.x=element_text(size = 14))
plot(z)
  
