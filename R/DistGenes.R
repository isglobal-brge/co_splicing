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