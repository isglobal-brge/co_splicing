#getDataGroup function must be loaded.
        #g1 , g2 & batchmat contain square matrices of the same dimension
        # g1 & g2 cointains the distances between individuals about the 
                #expression of concret genes 1 and 2.
        #batchmat contains the batch effect among all genes
        # group: the factor vector with the group of each individual in order
                #The relation length(group)*(length(group)-1)/2 = length(x)
                #must be satisfied

#It returns the maximum value between the differences of the values 
#from mantel statistic applyed for each group.

mantelcorGroup  <- function(g1, g2, batchmat, group) {
        
        list1 <- getDataGroup(x = g1 ,group = group)
        list2 <- getDataGroup(x = g2 ,group = group)
        list3 <- getDataGroup(x = batchmat ,group = group)
        
        MAA <- mantelcor(list1$AA, list2$AA, list3$AA, use = "complete.obs")
        MAB <- mantelcor(list1$AB, list2$AB, list3$AB, use = "complete.obs")
        MBB <- mantelcor(list1$BB, list2$BB, list3$BB, use = "complete.obs") 
        
        maxim <- max(abs(MAA - MAB), abs(MAA - MBB), abs(MAB - MBB) )
        maxim
}