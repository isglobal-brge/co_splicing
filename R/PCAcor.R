#'  PCA first component correlation
#' 
#' The function performs a partial correlation test between two dataframes. Each dataframe contains the exon expression (rows)
#' of a particular gene in a sample of individuals (columns). A third dataframe with the same number of columns (individuals) is
#' added to be used as a control parameter. Its rows refer to covariables which affect the conditions of the experiment. On each 
#' data.frame individuals must appear on the same order.
#' 
#' @param exprg1 Dataframe with exon expression on a sample of individuals.
#' @param exprg2 The same as exprg1 but with the exon expression of another gene. Notice the number of rows in exprg1 & exprg2
#' can differ as different genes has different number of exons.
#' @param cov The dataframe with the covariables due to conditions on the experiment.
#' @return Returns the partial correlation coefficient from the scores of the first principal components that result from
#' each dataframe, using argument cov as the set of controlling variables.   
#' @export 

PCAcor<-function(exprg1, exprg2, cov, ...)
{
        subdata<-lapply(1:ncol(exprg1), function(x) exprg1[,x]/sqrt(exprg1[,x]%*%exprg1[,x]))
        exprg1<-do.call(cbind,subdata)
        
        subdata<-lapply(1:ncol(exprg2), function(x) exprg2[,x]/sqrt(exprg2[,x]%*%exprg2[,x]))
        exprg2<-do.call(cbind,subdata)
        
        sc1<- prcomp(exprg1)$rotation[,1]
        sc2<- prcomp(exprg2)$rotation[,1]
        
        pcor.test(sc1,sc2,cov)
}

