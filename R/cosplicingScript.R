
#libraries
library(parallel)
library(MASS)
library(vegan)

#numero de cores
CORES<-30

############
###datos
############

#exon data for a given tissue
expr<-read.table(XXX.txt,as.is=TRUE,header=TRUE)


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

DistGenes<-mclapply(intgenes, function(ii)
{

  print(ii)

  gg<-listgenes[ii]
  exprapp<-as.matrix(exprdat[which(genelab==gg),])


  subdata<-lapply(1:ncol(exprapp), function(x) exprapp[,x]/sqrt(exprapp[,x]%*%exprapp[,x]))


  ##distace matrix 
  disAPP<-unlist(data.frame(lapply(1:length(subdata), function(n) lapply(1:length(subdata), function(m) subdata[[n]]%*%subdata[[m]]))))

  disAPP<-matrix(disAPP,ncol=length(subdata))
  disAPP
  },mc.cores=CORES)


################
# Este es el calculo de la matriz de correlacion entre todos los genes 
###############

##function to call the correlation matrix
getmatcor<-function(DistGenes)
{

  callMantelall<-lapply(1:(length(DistGenes)-1), function(sm1)
  {

    print(sm1)


    callMantelgene<-unlist(mclapply((sm1+1):length(DistGenes), function(sm2)
    {

       if(any(is.na(DistGenes[[sm1]])) |  any(is.na(DistGenes[[sm2]])))
       {
         out<-NA
       }else{
         mn<-mantel.partial(as.dist(DistGenes[[sm1]]),as.dist(DistGenes[[sm2]]),as.dist(disgeneGW),    permutations=1)
        out<-mn$statistic
      }
      out

    },mc.cores=CORES))

  callMantelgene

  })


  names(callMantelall)<-names(DistGenes)[-length(DistGenes)]


  #format results as a square matrix
  matcor<-mclapply(callMantelall, function(mn)
  {
    ln<-length(DistGenes)-length(mn)-1
    c(rep(0,ln),0.5,mn)
  },mc.cores= CORES)

  matcor<-do.call(rbind,matcor)

  matcor<-rbind(matcor,c(rep(0,nrow(matcor)),0.5))

  colnames(matcor)<-names(DistGenes)
  rownames(matcor)<-names(DistGenes)

  matcor<-matcor+t(matcor)

  save(matcor,file=paste("./GWsplicing/matcor",".RData",sep=lab[tissue])) 

}

##call the function
getmatcor(DistGenes)

