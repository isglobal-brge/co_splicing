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