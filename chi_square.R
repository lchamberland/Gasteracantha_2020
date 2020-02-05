setwd("~/Desktop/Desktopfolder/Gasteracantha/Gasteracantha_2019/Chisquare")
library(MASS)       # load the MASS package 
#tbl = table(survey$Smoke, survey$Exer) 
#tbl                 # the contingency table 
 
library(viridis)
color <- read.table("color.csv")
color
chisq.test(color) 



spine <- read.table("spines.csv")
spine
chisq.test(spine)

colorCAR <- read.table("colorCAR.csv")
colorCAR
chisq.test(colorCAR)

mosaicplot(colorCAR, color = viridis(), main = "Title") +
  scale_color_viridis(option="plasma",discrete=TRUE)



ggplot(data = mca1_obs_df, aes(x = Dim.1, y = Dim.2)) + geom_hline(yintercept = 0, 
                                                                   colour = "gray70") + geom_vline(xintercept = 0, colour = "gray70", alpha = 0.001) + 
  geom_point(colour = "gray50", alpha = 0.5, size=1) + 
  geom_density2d(colour = "gray80", alpha=0.5) + 
  geom_text(data = mca1_vars_df, aes(x = Dim.1, y = Dim.2, label = rownames(mca1_vars_df), colour = Variable), size=3) + 
  ggtitle("MCA plot of variables using R package FactoMineR") + 
  #scale_color_manual(values=c("dodgerblue4", "darkolivegreen4", "darkorchid3", "goldenrod1"))+
  scale_color_viridis(option="plasma",discrete=TRUE)+
  theme_bw()+
  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "gray70"))

dev.copy(pdf,'mca_haplo_new.pdf')
dev.off()