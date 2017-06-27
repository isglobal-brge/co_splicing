#'  Mantel test on different groups
#' 
#' The function uses getDataGroup function to perform mantel test on data from individuals that
#' belongs to the same group.
#' 
#' @param g1  The distance data vector for a particular gene.
#' @param g2 The distance data vector for another gene. 
#' @param batchmat The distance data vector containing the corrections due to experiment conditions.
#' The length of g1, g2 and batchmat must be the same.
#' @param group A factor with the group to which belongs each individual in the same order as they are on the matrix
#' from which the three first arguments belong. The function is performed so that the number of groups be 3 and the levels
#' of group be "AA", "AB" & "BB" in this order.
#' @return The function calculate the differences in mantel correlation value from each pair of groups and returns the 
#' maximum value between the absolute value of these differences.
#' @export 

mantelcorGroup  <- function(g1, g2, batchmat, group) {
        if (!is.factor(group)){stop("group variable must be a factor")}
        
        list1 <- getDataGroup(x = g1 ,group = group)
        list2 <- getDataGroup(x = g2 ,group = group)
        list3 <- getDataGroup(x = batchmat ,group = group)
        
        MAA <- try(mantelcor(list1$AA, list2$AA, list3$AA, use = "complete.obs"))
        MAB <- try(mantelcor(list1$AB, list2$AB, list3$AB, use = "complete.obs"))
        MBB <- try(mantelcor(list1$BB, list2$BB, list3$BB, use = "complete.obs")) 
        
        if(class(MAA)=="try-error" || class(MAB)=="try-error" || class(MBB)=="try-error"){
                return(NA)
        }
        maxim <- max(abs(MAA - MAB), abs(MAA - MBB), abs(MAB - MBB))
        maxim
}