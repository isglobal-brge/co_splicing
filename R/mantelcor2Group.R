#Levels of groups are A & B.
#This function acts on trymcGroup when group AA has less than 5 individuals.
mantelcor2Group  <- function(g1, g2, batchmat, group) {
        if (!is.factor(group)){stop("group variable must be a factor")}
        
        list1 <- getDataGroup(x = g1 ,group = group)
        list2 <- getDataGroup(x = g2 ,group = group)
        list3 <- getDataGroup(x = batchmat ,group = group)
        
        MA <- try(mantelcor(list1$A, list2$A, list3$A, use = "complete.obs"))
        MB <- try(mantelcor(list1$B, list2$B, list3$B, use = "complete.obs"))
        
        if(class(MA)=="try-error" || class(MB)=="try-error"){
                return(NA)
        }
        abs(MA-MB)
}