# 
# #libraries
# library(parallel)
# library(MASS)
# library(vegan)
# 
# # load fuctions
# source("getmatcor.R")
# source("mantelcor.R")
# source("getDataGroup.R")
# 
# #numero de cores
# CORES<-1
# 
# 
# ############
# ### get matrices having batch effect  
# ############
# 
# load("../data/disgeneGWexon_Testis.RData")
# 
# 
# ################
# # Este es el calculo de la matriz de correlacion entre todos los genes 
# ###############
# 
# ##call the function
# set.seed(1234)
# group <- as.factor(sample(c("AA", "AB", "BB"), 172, rep=TRUE, prob=c(0.64, 0.32, 0.04)))
# 
# n  <- length(group)
# xAA  <- xAB <- xBB <- matrix(FALSE, nrow=n, ncol=n)
# xAA[group=="AA"]  <- TRUE
# xAB[group=="AB"]  <- TRUE
# xBB[group=="BB"]  <- TRUE
# 
# 
# M <- mantelcor(DD[[1]], DD[[2]], D3, use="complete.obs")
# 
# matelcorGroup  <- function(x, y, z, group) {
# 
# mm <- getDataGroup(DD[[1]], group)
#   
# MAA 
# MAB
# MBB
# 
# 
# dd <- read.delim("../data/exon_Testis.txt.txt")
# batch <- getBatchMatrix(dd)
# 
# D3[1:10]
# batch[upper.tri(batch, diag = FALSE)][1:10]
# batch[lower.tri(batch, diag = FALSE)][1:10]
# 
# 
# dd[,1][1:10]
# 
# 
# o <- getDistanceGenes(dd)
# o[[1]][upper.tri(o[[1]], diag = FALSE)][1:10]
# o[[1]][lower.tri(o[[1]], diag = FALSE)][1:10]
