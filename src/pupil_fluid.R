
# Description -------------------------------------------------------------

### Generate fluid variables for students, things that change 
### every marking period (credits enrolled, quarterly_gpa, on campus)


# sources -----------------------------------------------------------------

source("src/generators.R")

student.pop = 8e3 # set population size
dists = distGen(n = student.pop) # generate distributions

gpa = function(n = 4e2){ # model quarterly GPA
  # use bell curve + white noise scaled to 4
  tmp = linMap(dists$bell * noiseGenerator(n), 0 , 4)
  return(tmp)
}

creditHours = function(n = 4e2) { # model credit hour enrollment
  # use high tailed dist (0,24)
  tmp = linMap(dists$high.skew * noiseGenerator(n), 0, 22)
  return(tmp)
}

emailCount = function(n = 4e2) { # model count of emails sent to professors
  # use decay (0,60)
  tmp = linMap(dists$decay * noiseGenerator(n), 0, 60)
  return(tmp)
}

pupil_fluid = function(n = 4e2) {
  tmp = data.frame(
    gpa = gpa(n),
    creditHours = creditHours(n),
    emailCount = emailCount(n)
  )
  return(tmp)
}

