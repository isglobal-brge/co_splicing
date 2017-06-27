#'  Mantel correlation
#' 
#' The function performs a mantel correlation test between two data vectors. A third vector is
#' added to be used as a control parameter. It's a correction that consider the experiment conditions
#' in which the first two vectors where extracted. 
#' 
#' @param D1 Vector with the distance between individuals. Each element measures the distances in gene expression
#' at exon level between two individuals, of a particular gene.
#' @param D2 The same as D1, but with the distances corresponding to another gene. 
#' @return Returns the partial correlation coefficient from the correlations between each pair of vectors
#' on the arguments.
#' @export 

mantelcor<-function(D1,D2,D3, ...)
{
  xdis <- D1
  ydis <- D2
  zdis <- D3
    
  rxy <- cor(xdis, ydis, ...)
  rxz <- cor(xdis, zdis, ...)
  ryz <- cor(ydis, zdis, ...)
  out <-  (rxy - rxz * ryz)/sqrt(1 - rxz * rxz)/sqrt(1 - ryz * ryz)

  out
}