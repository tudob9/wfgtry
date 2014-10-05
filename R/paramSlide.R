# this studies single-transformation-single-shape problems:
# it varies a single parameter, holding the others constant.

# these are about single-trafo specs

source("wfgEvalAndWrap.R")

# vary transformation parameters and observe how HV changes

specStudy = function(spec, algo) {
  w = wfgWrap(2, spec)
  return ( averagedHV(w, 1000, algo) )
}

# specStudy( c(sLinear) )

# tPoly: the plotIndividualsInObjSpace change dramatically at a 1:
# plotIndivsInObjSpace(c(tPoly, NA, 0.5, sLinear))
# plotIndivsInObjSpace(c(tPoly, NA, 1.5, sLinear))
paramAnalysis = function(trafo, par.name, par.values, par.show, additional.pars, algo=randomsearch, ...) {
    # par.show will be the matrix's column 1 entries and the plot's horizontal axis
  len = length(par.values)
  m = matrix(NA, nrow=len, ncol=2)
  for (i in 1:len) {
    spec = c(trafo, par.values[i], additional.pars, sLinear)
    names(spec) = c(NA, par.name, names(additional.pars), NA)
    hv = specStudy(spec, algo)
    m[i, ] = c(par.show[i], hv)
  }
  print(m)
  plot(m, ...)
  return (m)
}

# where = seq(-15, 15, len=10)
where = -15:15
set.seed(1)
paPolyAlpha = paramAnalysis(tPoly, "alpha", 2**where, where, c(), xlab="", ylab="") # very good plot showing that 2**-1 to 2**7 has high HV (ie. is easy)

baPng("hvPolyAlpha", plot(paPolyAlpha, xlab="", ylab="", pch=3) )
  # note that the plot horiz.axis (matrix col 1) shows the *display* values. the real parameters (2**where) are usually too unevenly spaced to plot
paramAnalysis(tPoly, "alpha", 2**where, where, c(), wrapNsga2)

to = seq(0.101, 0.8, len=10)
paramAnalysis(tFlat, "to", to, to, c(from=0.1)) # very interesting to see the objective-space before the dominated points are removed.
paramAnalysis(tFlat, "to", to, to, c(from=0.1), wrapNsga2) # nsga2 drops suddenly from 116 to 113

# tParam

where = seq(0.0001, 0.9999, len=10)
paramAnalysis(tLinear, "zero.loc", where, where, c())
paramAnalysis(tLinear, "zero.loc", where, where, c(), wrapNsga2)

where = seq(0.1, 0.9, len=10)
paramAnalysis(tDecept, "opti.loc", where, where, c()) # as expected no trends visible
paramAnalysis(tDecept, "opti.loc", where, where, c(), wrapNsga2)

# tDecept aperture
where = seq(0.0001, 0.25, len=10)
paramAnalysis(tDecept, "aperture", where, where, c()) # increases
paramAnalysis(tDecept, "aperture", where, where, c(), wrapNsga2)

where.show = seq(-8, -2, len=10)
where.val = 2**(where.show)
where.show
where.val 
paramAnalysis(tDecept, "deceptive.value", where.val, where.show, c()) # strong downward. this may be suprising because for an iterative algorithm a low deceptive value makes the problem of leaving the deceptive regions (to reach the 'true' optimum (true is in '' because this is pareto)) harder. however, hypervolume does not care whether the true optimum was found or not as long as the objectives are small (and they are, in the deceptive regions). 
paramAnalysis(tDecept, "deceptive.value", where.val, where.show, c(), wrapNsga2) # decreasing too
paramAnalysis(tDecept, "deceptive.value", where.val, where.show, c(aperture=0.1), wrapNsga2) # interesting

where = 0:(len-1)
paramAnalysis(tMulti, "num.minima", 2**where, where, c(hill.size=1)) # increases
paramAnalysis(tMulti, "num.minima", 2**where, where, c(hill.size=1), wrapNsga2)

where = seq(0, 7, len=10)
paramAnalysis(tMulti, "hill.size", 2**where, where, c(num.minima=100)) # HV increases with hillsize? maybe higher hills also have lower valleys?
paramAnalysis(tMulti, "hill.size", 2**where, where, c(num.minima=100), wrapNsga2) # no trend

# tSum

# tNonsep
where = 1:8
paramAnalysis(tNonsep, "degree", where, where, c()) # no trend
paramAnalysis(tNonsep, "degree", where, where, c(), wrapNsga2) # no trend. strange, it should make the problem harder




