

setwd("~/Desktop/Desktopfolder/Gasteracantha/Gasteracantha_2019/Gasteracantha Mantel/Caribbean")
#install.packages("ade4")
#install.packages("ggplot")
#install.packages("geosphere", repos="http://R-Forge.R-project.org")
library(ade4)
library(geosphere)
library(ape)
################################################################################################
###################### Read fasta and locality files and create distance files 
################################################################################################
#Geo
geoframe <- read.csv(file ="localities.csv", header=TRUE, row.names = 1)
# create distance matrix
geodist <- distm(geoframe[,c('Longitude','Latitude')], geoframe[,c('Longitude','Latitude')], fun=distVincentyEllipsoid)

write.csv(geodist, "geodist_car.csv")

#DNA
dna <- ape::read.dna("Gcancri.fas", format="fasta")

gendist <-dist.dna(dna, model = "TN93", as.matrix = TRUE)

write.csv(gendist, "gendist.csv")


################################################################################################
###################### Read matrices and set as disances
################################################################################################

geodist <- read.csv(file ="geodist_car.csv", header=TRUE, row.names = 1)
gendist <- read.csv("gendist.csv", header=TRUE, row.names = 1)

geo <- as.dist(geodist)  
geo

gen <- as.dist(gendist)  
gen

geogen <- as.array(gen,geo)

str(geogen)

################################################################################################
###################### Statisics
################################################################################################

mod1 <- lm(gen ~ geo)

M.lm<-gls(gen~geo)
summary(M.lm)
#heterogeneity of residuals based on values of geo 

M.lm2<-gls(gen~geo, weights=varExp(~geo))


res <- signif(residuals(mod1))
pre <- predict(mod1)

coef(lm(gen ~ geo))
summary(mod1)

summary(lm(gen ~ geo))


################################################################################################
###################### assumttoions
################################################################################################
#predicted y observed values vs predicted y values

library(ggplot2)
#1st assumption
qplot(pre, gen) + geom_abline(slope=1)
dev.copy(pdf,'pre-obs.pdf')
dev.off()

#2nd assumption - homogeneity of errors 
#residuals vs x values 
qplot(geo, res)
dev.copy(pdf,'res.pdf')
dev.off()
#normality of resuduals
hist(res)
dev.copy(pdf,'res-hist.pdf')
dev.off()
#
qqnorm(res)

qqline(res)
dev.copy(pdf,'resline.pdf')
dev.off()



################################################################################################
###################### MantelTest
################################################################################################
library(ade4)


mantel.rtest(geo, gen, nrepet = 100000)
################################################################################################
###################### Plot
################################################################################################

plot(geo, gen, abline(lm(gen ~ geo)), col=("purple3"), pch=16)
legend("topright", bty="n", legend= "y = 6e-09x + 0.006
       R-sq = 0.2807
       p = <0.0001")
dev.copy(pdf,'GcancriIBD.pdf')
dev.off()

################################################################################################
###################### Plot with two different color dots
################################################################################################

abline(lm(gen ~ geo))
plot(geo, gen, col=ifelse(gen>=0.035,"orange","turquoise"), pch=16)
#lines(geo)

legend("bottomright", bty="n", legend= "R-sq = 0.2807
       p = <0.0001")

plot(geo, gen, abline(lm(gen ~ geo)), col=ifelse(gen>=0.035,"goldenrod","purple"), pch=16)



#install.packages("shiny")
#install.packages("ggplot2")
#install.packages("plotly")

library(plotly)
packageVersion('plotly')

plot_ly(geo, gen, abline(lm(gen ~ geo)), col=ifelse(gen>=0.035,"orange","purple"), pch=16)

install.packages("ggplot2")


