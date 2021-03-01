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


ffd4_nona <- na.omit(ffd4)
aggregate(ffd4_nona$FFD,         # Median by group
          list(ffd4_nona$Name),
          median)

library(forcats)
z_names <- ggplot(ffd4, aes(x=reorder(Name, FFD, na.rm = TRUE), y = FFD))+
  geom_boxplot()+
  theme_classic() +
  ylab("FFD")+ 
  xlab("Species") +
  theme(text = element_text(family = "serif"),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=14),
        axis.text.x=element_text(size = 12, angle = 45, vjust = 1, hjust = 1, face = "italic"),
        axis.title.x=element_text(size = 14),
        plot.margin = margin(0.5,0,0,1.2, "cm"))
plot(z_names)
