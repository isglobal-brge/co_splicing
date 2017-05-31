##exprg1 y exprg2 son dos tablas con la expresión exónica de dos genes distintos
##cov contiene datos que tienen en cuenta el batch effect del experimento
## Las 3 tablas deben de compartir los mismos individuos en el mismo orden
PCAcor<-function(exprg1, exprg2, cov, ...)
{
        subdata<-lapply(1:ncol(exprg1), function(x) exprg1[,x]/sqrt(exprg1[,x]%*%exprg1[,x]))
        exprg1<-do.call(cbind,subdata)
        
        
        subdata<-lapply(1:ncol(exprg2), function(x) exprg2[,x]/sqrt(exprg2[,x]%*%exprg2[,x]))
        exprg2<-do.call(cbind,subdata)
        
        sc1<- prcomp(exprg1)$rotation[,1]
        sc2<- prcomp(exprg2)$rotation[,1]
        
        pcor.test(sc1,sc2, cov)
        
}

