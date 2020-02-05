
#devtools::install_github("gtonkinhill/rhierbaps")

# install.packages('rhierbaps')
library(rhierbaps)
library(ggtree)
library(phytools)
library(ape)

setwd("~/Desktop/Desktopfolder/Gasteracantha/Gasteracantha_2019/Rhierbaps")
#We first need to load a multiple sequence alignment in fasta format. We can then generate the required SNP matrix.
fasta.file.name <- system.file("extdata", "seqs.fa", package = "rhierbaps")
#fasta.file.name <- 
snp.matrix <- load_fasta("Gast_car-SA_COI_Gkhuli_forbeast.fas")

#It is also possible to load an ape DNAbin object. Here me make use of the woodmouse dataset in ape.
#data(woodmouse)
#woodmouse.snp.matrix <- load_fasta(woodmouse)

#To run hierBAPS with 2 levels and 20 initial clusters we run
hb.results <- hierBAPS(snp.matrix, max.depth = 2, n.pops = 20, quiet = TRUE)
head(hb.results$partition.df)
#>   Isolate level 1 level 2
#> 1       1       1       1
#> 2       2       1       1
#> 3       3       1       1
#> 4       4       2       5
#> 5       5       3       9
#> 6       6       3       9

#Plotting results 
#Load newik tree
newick.file.name <- system.file("extdata", "seqs.fa.treefile", package = "rhierbaps")
iqtree <- phytools::read.newick("Gast_COI_new.phy")

#A simple coloured tree allows us to see the top level cluster assignment from hierBAPS.
gg <- ggtree(iqtree, layout = "circular")
gg <- gg %<+% hb.results$partition.df
gg <- gg + geom_tippoint(aes(color = factor(`level 1`)))
gg

plot_sub_cluster(hb.results, iqtree, level = 1, sub.cluster = 9)
hb.results$lml.list


write.csv(hb.results$partition.df, file = file.path("~/Desktop/Desktopfolder/Gasteracantha/Gasteracantha_2019/Rhierbaps", "hierbaps_partition.csv"), 
          col.names = TRUE, row.names = FALSE)

save_lml_logs(hb.results, file.path("~/Desktop/Desktopfolder/Gasteracantha/Gasteracantha_2019/Rhierbaps", "hierbaps_logML.txt"))

