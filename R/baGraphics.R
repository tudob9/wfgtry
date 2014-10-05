source("wfgEvalAndWrap.R")
source("toPlot.R")

# trueFronts:

trueFront = function(name, spec, removeDominated=TRUE) {
  png(paste(sep="", "../baGraphics/", name, ".png"))
  set.seed(1)
  n = 1000
  m = matrix(NA, nrow=n, ncol=2)
  where = seq(0, 1, length.out=n)
  for(i in 1:n) {
    m[i, ] = wfgEval( c(where[i], where[i]), 2, true.front=TRUE, spec)
  }
  if(removeDominated) {
    m = nonDominated(m, 0)
  }
  m = m[order(m[, 1]), ]
  plot(m, pch=19, main="", xlab="", ylab="")
    # we would want to use type="L" and much fewer points, however we have to display a disconnected front.
  dev.off()
}
# trueFront("truefrontLinear", c(sLinear))
# trueFront("truefrontConvex", c(sConvex))
# trueFront("truefrontConcave", c(sConcave))
# trueFront("truefrontMixed", c(sMixed, sMixed))
# trueFront("truefrontDisc", c(sDisc, sDisc))
# trueFront("truefrontDiscDomi", c(sDisc, sDisc), removeDominated=FALSE)

# toPlots: 

baGraphics = function(name, spec) {
  png(paste(sep="", "../baGraphics/", name, ".png"))
  set.seed(1)
  toPlot22(spec, main="", xlab="", ylab="")
  dev.off()
}
baPng = function(name, expr) {
  png(paste(sep="", "../baGraphics/", name, ".png"))
  set.seed(1)
  eval(expr)
  dev.off()
}

# (1)
# baGraphics("linearFront", c(sLinear))
# baGraphics("poly5", c(tPoly, NA, 5, sLinear))
# baGraphics("flat", c(tFlat, from=0.4, to=0.9, sLinear))
# (2)
# baGraphics("param", c(tParam, NA, 0.9, sLinear))
# baGraphics("linear", c(tLinear, NA, 0.8, sLinear))
# baGraphics("decept", c(tDecept, aperture = 0.1, sLinear))
# (3)
# baGraphics("multi", c(tMulti, hill.size=2, num.minima=100, sLinear))
# baGraphics("sum", c(tSum, sLinear))
# baGraphics("nonsep", c(tNonsep, NA, 10, sLinear))
# (appendix)
# baGraphics("toPlotDisc", c(sDisc, sDisc))
