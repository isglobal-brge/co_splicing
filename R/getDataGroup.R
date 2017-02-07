
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
    vvv <- vv[upper.tri(vv, diag=FALSE)]
    ans[[i]] <- x[vvv==1]
  }
  names(ans) <- ll
  ans
}