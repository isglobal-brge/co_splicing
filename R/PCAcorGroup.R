#'  PCA first component correlation by 3 groups
#' 
#' The function performs the partial correlation test done in function PCAcor for two datasets and using argument cov as set of controlling variables.
#' This calculations are done by pulling away the individuals in three groups: "AA", "AB" & "BB".
#' 
#' @param exprg1 Dataframe with exon expression on a sample of individuals.
#' @param exprg2 The same as exprg1 but with the exon expression of another gene. Notice the number of rows in exprg1 & exprg2
#' can differ as different genes has different number of exons.
#' @param cov The dataframe with the covariables due to conditions on the experiment.
#' @return Returns the maximium absolute value of the differences in the partial correlation coefficient coming from each group.
#' @export 

PCAcorGroup  <- function(exprg1, exprg2, cov, group) {
        if (!is.factor(group)){stop("group variable must be a factor")}
        
        PCA_AA <- try(PCAcor(exprg1[,"AA"], exprg2[,"AA"], cov[,"AA"], use = "complete.obs"))
        PCA_AB <- try(PCAcor(exprg1[,"AB"], exprg2[,"AB"], cov[,"AB"], use = "complete.obs"))
        PCA_BB <- try(PCAcor(exprg1[,"BB"], exprg2[,"BB"], cov[,"BB"], use = "complete.obs")) 
        
        if(class(PCA_AA)=="try-error" || class(PCA_AB)=="try-error" || class(PCA_BB)=="try-error"){
                return(NA)
        }
        maxim <- max(abs(PCA_AA - PCA_AB), abs(PCA_AA - PCA_BB), abs(PCA_AB - PCA_BB))
        maxim
}