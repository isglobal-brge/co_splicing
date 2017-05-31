#It returns the maximum value between the differences of the values on PCAcor for each group
#It is needed that the levels are AA, AB, BB !!

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