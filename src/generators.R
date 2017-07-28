library(tidyverse)


# Linear Map --------------------------------------------------------------

# map pdf to a given range
linMap <- function(x, from, to) {
  # Shifting the vector so that min(x) == 0
  x <- x - min(x)
  # Scaling to the range of [0, 1]
  x <- x / max(x)
  # Scaling to the needed amplitude
  x <- x * (to - from)
  # Shifting to the needed level
  x + from
}

# Distribution Generators -------------------------------------------------
# Generate a distribution/shape once and use it with noiseGenerator() when needed
distGen <- function(n = 100) { # generate probability distributions
  # bell curve
  bell.set = rnorm(n, mean = 0, sd = 1)
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

# White Noise Generator, to be used at will, pass sample size and amplitude or spread %
noiseGenerator <- function(n = 100, amp = .2){ 
  # just a bit of random noise around 0.9-1.1
  return(runif(n, 1-(amp/2), 1+(amp/2)))
}

