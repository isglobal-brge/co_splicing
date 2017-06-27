#'  Mantel test on two groups
#' 
#' The function uses getDataGroup function to perform mantel test on data from individuals that
#' belongs to the same group. In this function two groups are considered: "A" & "B". The group "A" results from
#' joining the groups "AA" & "AB" from the factors used in mantelcorGroup. The group "B" is the group "BB".
#' 
#' @param g1  The distance data vector for a particular gene.
#' @param g2 The distance data vector for another gene. 
#' @param batchmat The distance data vector containing the corrections due to experiment conditions.
#' The length of g1, g2 and batchmat must be the same.
#' @param group A factor with the group to which belongs each individual in the same order as they are on the matrix
#' from which the three first arguments belong. The function is performed so that the number of groups be 2 and the levels
#' of group be "A" & "B" in this order.
#' @return The function calculate the absolute value of the difference in mantel correlation value from groups "A" & "B".
#' @export 

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