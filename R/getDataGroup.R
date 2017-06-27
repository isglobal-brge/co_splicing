#' Separate data into different groups
#' 
#' This function separate the distance between individuals data contained in x accordingly with 
#' his class on the variable group.
#' 
#' @param x  A vector with the distance between individuals data. The vector contains the lower
#' triangle of a distance matrix filled in the same order as the lower.tri() function.
#' @param group A factor with the group to which each individual of the distance matrix belong.
#' The order of individuals on this variable must be the same as the distance matrix. 
#' @return It returns a list with number of elements equal to the number of levels on group,
#' i.e, the number of classes. Each element contains a vector with the distances between individuals
#' belonging to the same group. The elements are stuck on the same order as they were on vector x.
#' @export 

getDataGroup <- function(x, group) {
  if (!is.factor(group))
    stop("group variable must be a factor") 
  ll <- levels(group)
  k <- length(ll)
  ans <- list()
  for (i in 1:k)
  {  
    v <- group == ll[i]
    vv <- outer(v,v)
    vvv <- vv[lower.tri(vv, diag=FALSE)]
    ans[[i]] <- x[vvv==1]
  }
  names(ans) <- ll
  ans
}