trymcGroup <- function(g1, g2, batchmat, group){
        
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
        
        #As the first group commonly has less individuals in case that the number is less
        #than 5 we join the group AA with AB.
        if(sum(group =="AA")<5){
                levels(group) = c("A","A","B")
                test <- try(mantelcor2Group(g1, g2, batchmat, group),silent = T)
                return(test)
        }
        
        test <- try(mantelcorGroup(g1, g2, batchmat, as.factor(group)),silent = T)
        if(class(test)=="try-error")
        {
                test<-NA
        }
        test
}