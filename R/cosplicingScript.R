
#libraries
library(parallel)
library(MASS)
library(vegan)

# load fuctions
source("getmatcor.R")

#numero de cores
CORES<-30

################
### get data
################

#exon data for a given tissue
expr <- read.table("data/XXX.txt", as.is=TRUE, header=TRUE)

# select gene of interest
genelab<- "es una variable con el nombre del gen para cada exon"
listgenes<-unique(genelab)

############
###esta es la matriz de distancia de batch effect  
############

#build distance matriz between individuals / batch effect

##get subject profiles as normalized vectors
subdata<-mclapply(1:ncol(exprdat), function(x) exprdat[,x]/sqrt(exprdat[,x]%*%exprdat[,x]), mc.cores=5)

##compute inner product between pairs of individuals
disgeneGW<-unlist(data.frame(mclapply(1:length(subdata), function(n) lapply(1:length(subdata), function(m) subdata[[n]]%*%subdata[[m]]),mc.cores=5)))

disgeneGW<-matrix(disgeneGW,ncol=length(subdata))


##########
#esta es una lista con todas las matrices de distancia para cada uno de los genes 
##########

intgenes<-1:length(listgenes)


################
# Este es el calculo de la matriz de correlacion entre todos los genes 
###############

##call the function
getmatcor(DistGenes)

