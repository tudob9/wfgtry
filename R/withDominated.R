withDominated = function(func) {
  set.seed(1)
  n = 1000
  m = matrix(NA, nrow=n, ncol=2)
  for(i in 1:n) {
    m[i, ] = func( runif(2) )
  }
  m = m[order(m[, 1]), ]
  plot(m, pch=3, main="", xlab="", ylab="")
}
baGraphicsWithDominated = function(name, spec) {
  png(paste(sep="", "../baGraphics/", name, ".png"))
  withDominated(wfgWrap(2, spec))
  dev.off()
}
# withDominated(wfgWrap(2, c(sConvex)))

# baGraphicsWithDominated("withDominatedSLinear", c(sLinear))
# baGraphicsWithDominated("withDominatedSConvex", c(sConvex))
# baGraphicsWithDominated("withDominatedSConcave", c(sConcave))

# baGraphicsWithDominated("withDominatedSMixed", c(sMixed, sMixed))
# baGraphicsWithDominated("withDominatedSDisc", c(sDisc, sDisc))

# baGraphicsWithDominated("withDominatedTNothing", c(sLinear))
# baGraphicsWithDominated("withDominatedTPoly", c(tPoly, NA, 5, sLinear))
# baGraphicsWithDominated("withDominatedTFlat", c(tFlat, from=0.4, to=0.9, sLinear))
# baGraphicsWithDominated("withDominatedTParam", c(tParam, NA, 0.9, sLinear))
# baGraphicsWithDominated("withDominatedTLinear", c(tLinear, NA, 0.8, sLinear))

# baGraphicsWithDominated("withDominatedTDecept", c(tDecept, aperture = 0.1, sLinear))
# baGraphicsWithDominated("withDominatedTMulti", c(tMulti, hill.size=2, num.minima=100, sLinear))
# baGraphicsWithDominated("withDominatedTSum", c(tSum, sLinear))
# baGraphicsWithDominated("withDominatedTNonsep", c(tNonsep, sLinear))
