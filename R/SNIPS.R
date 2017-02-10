SNIPS <- function(indnames, SNIPgroups, g1, g2, batchmat){
        
        #This two lines transform the rownames of the matrix selecting the second
        #string, which match with the labels from indnames.
        splitnames <- strsplit(rownames(SNIPgroups),"-")
        samenames <- sapply(splitnames, function(x)  x[[2]])
        #Select the names of the matrix that appears in indnames
        #The out of intersect follows the same order of indnames
        common <- intersect(indnames, samenames)
        
        #Returns the position of each member in common in the SNIP matrix files
        selsubs <- sapply(common, function(x) match(x,samenames))
        
        #Select only the files of the matrix that are in common, putting the files in the same order
        #as in common
        genosCommon <- SNIPgroups[selsubs,]
        
        #A vector of the length of indnames telling if each individual
        #appears on the matrix.
        groupCommon <- as.factor(as.numeric(indnames%in%common))
        
        #Now apply getDatagroup to sepparate the distances between individuals
        #of which we have the genotype from the others.
        #The output of each list will have $1 and $0 as names. The useful ones are
        # in $1
        g1new <- getDataGroup(x = g1, group = groupCommon)
        g2new <- getDataGroup(x = g2, group = groupCommon)
        batchmatnew <- getDataGroup(x= batchmat, group = groupCommon )
        
        ans <- sapply(X = 1:ncol(genosCommon), function(x) trymcGroup(g1new[[2]], g2new[[2]], batchmatnew[[2]], genosCommon[,x]))
        
        names(ans) <- colnames(genosCommon)
        ans
}