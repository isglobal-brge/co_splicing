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