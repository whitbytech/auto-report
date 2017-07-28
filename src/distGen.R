library(tidyverse)

distGen <- function(n = 100) { 
  # bell curve
  bell.set = rnorm(n,sd = 1)
  # low-end tendency
  low.set = rbeta(n, 3, 12)
  # high-end tendency
  high.set = rbeta(n,7,3)
  # uniform
  unif.set = runif(n)
  # long high tail unimodal
  gamma.set = rgamma(n,2)
  # discrete decay
  rg = rgeom(n,0.3)
  
  return(
    list(
      bell = bell.set,
      low.skew = low.set,
      high.skew = high.set,
      unif = unif.set,
      high.tail = gamma.set,
      decay = rg
    )
  )
}
