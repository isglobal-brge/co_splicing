getDistanceGenes <- function(data, colGene=1, mc.cores=1){
  temp <- data[ , colGene]
  temp2  <- unlist(lapply(strsplit(as.character(temp), "\\."), function(x) x[1]))
  listgenes <- unique(temp2)
  intgenes <- 1:length(listgenes)
  exprdat <- data[ , -c(colGene)]
  ans <- mclapply(intgenes, function(ii) {
    print(ii)
    gg<-listgenes[ii]
    exprapp<-as.matrix(exprdat[which(temp2==gg),])
    subdata<-lapply(1:ncol(exprapp), function(x) exprapp[,x]/sqrt(exprapp[,x]%*%exprapp[,x]))
    ##distace matrix 
    disAPP <- unlist(data.frame(lapply(1:length(subdata), function(n) lapply(1:length(subdata), function(m) subdata[[n]]%*%subdata[[m]]))))
    disAPP <- matrix(disAPP, ncol=length(subdata))
    disAPP}, mc.cores=mc.cores)
   ans
}
