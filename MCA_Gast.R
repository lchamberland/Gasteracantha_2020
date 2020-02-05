#install.packages("FactoMineR")
setwd("~/Desktop/Desktopfolder/Gasteracantha/Gasteracantha_2019/MCA_R")
# load packages
require(FactoMineR)
require(ggplot2)
# load data tea
#data(tea)
# select these columns
#newtea = tea[, c("Tea", "How", "how", "sugar", "where", "always")]
# take a look
#head(newtea)

traits <- read.csv("traits.csv", header=TRUE, row.names = 1)
head(traits)
traits
newtraits = traits[, c("color", "spines", "island", "Haplotype_COI")]
head(traits)

# number of categories per variable
cats = apply(newtraits, 2, function(x) nlevels(as.factor(x)))
cats
apply

# apply MCA
mca1 = MCA(newtraits, graph = FALSE)

# table of eigenvalues
mca1$eig

# column coordinates
head(mca1$var$coord)

# row coordinates
head(mca1$ind$coord)

# data frames for ggplot
mca1_vars_df = data.frame(mca1$var$coord, Variable = rep(names(cats), 
                                                         cats))
mca1_obs_df = data.frame(mca1$ind$coord)

# plot of variable categories
ggplot(data = mca1_vars_df, aes(x = Dim.1, y = Dim.2, label = rownames(mca1_vars_df))) + 
  geom_hline(yintercept = 0, colour = "gray70") + geom_vline(xintercept = 0, 
                                                             colour = "gray70") + geom_text(aes(colour = Variable)) + ggtitle("MCA plot of variables using R package FactoMineR") +
  theme_bw()


library(viridis)
# MCA plot of observations and categories
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


# default biplot in FactoMineR
plot(mca1)
# MCA plot of observations and categories
ggplot(data = mca1_obs_df, aes(x = Dim.1, y = Dim.2)) + geom_hline(yintercept = 0, 
colour = "gray70") + geom_vline(xintercept = 0, colour = "gray70") + geom_point(colour = "gray50", 
alpha = 0.7) + geom_density2d(colour = "gray80") + geom_text(data = mca1_vars_df, 
                                                                                                                                                                                                        aes(x = Dim.1, y = Dim.2, label = rownames(mca1_vars_df), colour = Variable)) + 
  ggtitle("MCA plot of variables using R package FactoMineR") + scale_colour_discrete(name = "Variable")

dev.copy(pdf,'mca_withoutcluster.pdf')
dev.off()

# default biplot in FactoMineR
plot(mca1)

#install.packages("factoextra")

library(factoextra)
eig.val <- get_eigenvalue(mca1)

fviz_screeplot(mca1, addlabels = TRUE, ylim = c(0, 45))
dev.copy(pdf,'screeplot.pdf')
dev.off()

# head(eig.val)
fviz_mca_var(mca1, choice = "mca.cor", 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())
dev.copy(pdf,'variables.pdf')
dev.off()
fviz_mca_var(mca1, choice = "quanti.sup",
             ggtheme = theme_minimal())
fviz_mca_ind(mca1, 
             label = "ind.sup", #Show the label of ind.sup only
             ggtheme = theme_minimal())
