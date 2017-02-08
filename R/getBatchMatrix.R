getBatchMatrix <- function(data, colGene = 1, mc.cores=1) {

  ##get subject profiles as normalized vectors
  exprdat <- data[ , -c(colGene)]
  subdata <- mclapply(1:ncol(exprdat), function(x) exprdat[,x]/sqrt(exprdat[,x]%*%exprdat[,x]), 
                      mc.cores=mc.cores)
  
  ##compute inner product between pairs of individuals
  disgeneGW <- unlist(data.frame(mclapply(1:length(subdata), function(n) lapply(1:length(subdata), function(m) subdata[[n]]%*%subdata[[m]]),
                                        mc.cores=mc.cores)))
  
  ans <- matrix(disgeneGW, ncol=length(subdata))
  ans
}
