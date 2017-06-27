#'  PCA first component correlation by 3 groups
#' 
#' The function performs the partial correlation test done in function PCAcor for two datasets and using argument cov as set of controlling variables.
#' This calculations are done by pulling away the individuals in two groups: "A" & "B".
#' 
#' @param exprg1 Dataframe with exon expression on a sample of individuals.
#' @param exprg2 The same as exprg1 but with the exon expression of another gene. Notice the number of rows in exprg1 & exprg2
#' can differ as different genes has different number of exons.
#' @param cov The dataframe with the covariables due to conditions on the experiment.
#' @return Returns the absolute value of the difference in partial correlation coefficient coming from each group.
#' @export 

PCAcor2Group  <- function(exprg1, exprg2, cov, group) {
        if (!is.factor(group)){stop("group variable must be a factor")}
        
        PCA_A <- try(PCAcor(exprg1[,"A"], exprg2[,"A"], cov[,"A"], use = "complete.obs"))
        PCA_B <- try(PCAcor(exprg1[,"B"], exprg2[,"B"], cov[,"B"], use = "complete.obs")) 
        
        if(class(PCA_A)=="try-error" || class(PCA_B)=="try-error"){
                return(NA)
        }
        dif <- abs(PCA_A - PCA_B)
        dif
}