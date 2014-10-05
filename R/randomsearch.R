source("wfgUtil.R")

randomdirection = function(dim) { # using Marsaglia's method
  x = rnorm(dim)
  r = sqrt(sum(x^2))
  return (x/r)
}
# example:
# m = matrix(NA, nrow=100, ncol=2)
# for (i in 1:100) {
#    m[i, ] = randomdirection(2)
# }
# plot(m)
randomsearch = function(func, evals=1000, inDim=5) {
  # cat("randomsearch with ", evals, " evaluations\n")
  points = floor(sqrt(evals)) # heuristic
  outDim = 2

  pars = matrix(rep(NA, points*inDim), nrow=points)
  values = matrix(rep(NA, points*outDim), nrow=points)

  # init
  # print("init")
  for(i in 1:points) {
    pars[i, ] = runif(inDim, 0, 1)
    values[i, ] = func(pars[i, ])
    # print(values[i, ])
  }

  # iter
  # print("iter")
  evalsRemaining = evals - points
  for (i in 0:(evalsRemaining-1)) {
    where = i%%points+1
    # cat("where: ", where, "\n")
    stepsize = rexp(inDim, rate= evals/points ) # random stepsize heuristic. 
                          # this is such that number of generations times average stepsize is 1.
    # cat("step ", stepsize, "\n")
    dir = randomdirection(inDim)
    # cat("dir ", dir, "\n")
    new.par = to01( pars[where, ] + dir * stepsize)
    new.value = func(pars[where, ])
    # cat("old point: ", pars[where, ], "\n")
    # cat("new point: ", new.par, "\n")
    # cat("old value: ", values[where, ], "\n")
    # cat("new value: ", new.value, "\n")
    if( all(new.value <= values[where, ])) {
      # cat("BETTER OR EQUAL\n")
      pars[where, ] = new.par
      values[where, ] = new.value
    } else {
      # cat("WORSE\n")
      # cat(".")
    }
  }
  mco = list(par=pars, value=values)
  return(mco)
}
