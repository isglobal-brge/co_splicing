#The function does two operations:
        #1. If there are NA in variable group it quit the indviduals which are not classified
                #Then it uses getDataGroup to extract only the values from g1, g2 & batchmat
                #corresponding to individuals which are classified.
                #Also it transform variable group to a factor with levels(group) <-c("AA", "AB", "BB") 
        #2. If further the number ofindividuals from group AA are less than 5 it joins individuals
                #from groups AA &AB.
        #If none of the previous conditions are met the only thing the function does is
                # transforming variable group to a factor with levels(group) <-c("AA", "AB", "BB")

#The output is a list with the news g1, g2, batchmat, group called with the same name.
rejoingroup <- function(g1, g2, batchmat, group){
        
        if(any(is.na(group))==T){
                NAS <- factor(as.numeric((complete.cases(group))))
                G1 <- getDataGroup(x = g1, group = NAS)
                G2 <- getDataGroup(x = g2, group = NAS)
                B <- getDataGroup(x= batchmat, group = NAS)
                
                g1 <- G1[[2]]
                g2 <- G2[[2]]
                batchmat <- B[[2]]
                group <- group[complete.cases(group)]
        }
        group <- as.factor(group)
        levels(group) <-c("AA", "AB", "BB")
        
        if(sum(group =="AA")<5){
                levels(group) = c("A","A","B")
        }
        ans <- list("g1"= g1, "g2" =g2, "batchmat"=batchmat, "group"=group)
}