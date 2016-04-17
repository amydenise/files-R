walsh.avg <- function(z){
  sort.z = sort(z)
  tmp = array(dim=c(length(z),length(z)))
  
  for (i in 1:length(z))
  {
    avg = (sort.z[i]+sort.z)/2
    for(j in 1:length(z))
    {
      tmp[j,i] <- avg[j]
    }
  }
  tmp[upper.tri(tmp,diag=F)] <- 0
  tmp
}

z=c(.147,.08, -.01, -.43, -.49, -.59, -.62, -.952, -1.022)

walsh.avg(z)