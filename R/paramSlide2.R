source("wfgEvalAndWrap.R")
source("studies.R")
source("baGraphics.R")

# these are about the 4-trafo problem

wrapAndAverageHVspecific = function(shape, alpha, loc.true.opt, aperture, val.decept, num.minima, height, degree) {
  func = wfgWrap(2, c(
    tPoly, NA, alpha,
    tDecept, NA, loc.true.opt, aperture, val.decept,
    tMulti, NA, num.minima, height, loc.true.opt,
    tNonsep, NA, degree,
    shape
  ))
  return ( averagedHV(func, 1000) )
}

wrapAndAverageHVspecific(sLinear, 2**-15, NA, NA, NA, NA, NA, NA)

shape = sLinear


where = -15:15
m = matrix(NA, 31, 2)
for (i in seq(along=where)) {
  polyAlpha = 2**where[i]
  func = wfgWrap(2, c(tPoly, NA, polyAlpha, tDecept, tMulti, tNonsep, shape))
  res = averagedHV(func, 1000)
  m[i, ] = c(where[i], res)
}
baPng("hvPolyAlphaLogScale", plot(m, xlab="", ylab=""))

# where = seq(0.0001, 0.9999, length.out=10)
# m = matrix(NA, 10, 2)
# for (i in seq(along=where)) {
#   loc.true.opt = where[i]
#   func = wfgWrap(2, c(tPoly, tDecept, NA, loc.true.opt, tMulti, tNonsep, shape))
#   res = averagedHV(func, 1000)
#   m[i, ] = c(where[i], res)
# }
# baPng("hvDeceptLocTrueOpt", plot(m, xlab="", ylab=""))


