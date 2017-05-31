#It returns the maximum value between the differences of the values on PCAcor for each group
#It is needed that the levels are A, B !!

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