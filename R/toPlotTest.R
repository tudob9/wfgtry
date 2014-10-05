source("toPlot.R")
source("wfgSuite.R")
testfunc = function(x) {
  # single objective:
  # res = sum(x^2)

  # two objectives:
  # res = c( x[1]-x[2], x[1]-x[2] )     # agreeing objectives
  # res = c( abs(x[1]-0.2), abs(x[2]-0.6) )       # non-conflicting objectives
  # res = c( abs(x[1]-0.8), abs(x[2]-0.1) )       # non-conflicting objectives
  # res = c( x[1]-x[2], x[2]-x[1] )    # disagreeing objectives. an example where nsga2 has no preferences.
  # res = c( sum(x^2), sum((x-1)^2) )    # an example where nsga2 chooses points that allow the decision-maker to weigh the objectives against eachother
  res = c( sum((x-0.3)^2), sum((x-0.7)^2) ) # shows that e.g. a bad obj1 is not considered if it doesn't at the same time offer a good obj2.

  # three objectives:
  # redOpt = c(1, 0, 0, 0, 0); 
  # greenOpt = c(cos(2*pi/3), sin(2*pi/3), 0, 0, 0)
  # blueOpt = c(cos(4*pi/3), sin(4*pi/3), 0, 0, 0)
  # res = c( sum((x-redOpt)^2), sum((x-greenOpt)^2), sum((x-blueOpt)^2) ) # shows that nsga2 offers the decision-maker mostly the scale between 2 extremes (not 3)
  # res = c( sum((x-redOpt)^2), sum((x-greenOpt)^2), 0 )
  # res = c( sum((x-redOpt)^2), 0, sum((x-blueOpt)^2) ) # strange nsga2 result. only (1.0, ~0.0)

  return(res)
}
toPlot(testfunc)

justConcave = wfgWrap(M, c(sConcave) )
toPlot(justConcave)

set.seed(1)
toPlot(wfg1)
toPlot(wfg4)
toPlot(wfg5)
toPlot(wfg6)
toPlot(wfg7)
toPlot(wfg9)

toPlot(wfgWrap(2, c(sLinear) ) )
  toPlot(wfgWrap(2, c(sConvex) ) )
  toPlot(wfgWrap(2, c(sConcave) ) )
toPlot(wfgWrap(2, c(tPoly, NA, 0.3, sLinear) ) )
toPlot(wfgWrap(2, c(tPoly, NA, 5, sLinear) ) )

toPlot(wfgWrap(2, c(sLinear) ) )
toPlot(wfgWrap(2, c(tDecept, sLinear) ) )
toPlot(wfgWrap(2, c(tDecept, deceptive.value = 0.1, sLinear) ) )
toPlot(wfgWrap(2, c(tDecept, aperture = 0.1, sLinear) ) )   # very
toPlot(wfgWrap(2, c(tDecept, aperture = 0.25, sLinear) ) )  # nice

toPlot22( c(sLinear) )
toPlot22( c(sConvex) )
toPlot22( c(sConcave) )
toPlot22( c(sLinear, 1, sDisc) )
toPlot22( c(sConvex, 1, sDisc) )
toPlot22( c(tMulti, sLinear) )
toPlot22( c(tFlat, sConvex) )
toPlot22( c(tPoly, sConvex) )
toPlot22( c(tDecept, sConvex) )

toPlot22( c(tParam, sLinear) ) # !  (approx 1/50th)
toPlot22( c(tParam, NA, 0.9, sLinear) ) # !
toPlot22( c(tLinear, sLinear) ) # !
toPlot22( c(tLinear, NA, 0.8, sLinear) ) # !

toPlot22( c(tFlat, sLinear) ) # too small to see
toPlot22( c(tFlat, from=0.4, to=0.9, sLinear) ) # good
toPlot22( c(tFlat, 1, from=0.4, to=0.9, tNone, sLinear) ) # good
toPlot22( c(tNone, 1, tFlat, from=0.4, to=0.9, sLinear) ) # strange that this is not horizontal

toPlot22( c(tFlat, from=0.4, to=0.9, sLinear) ) # good

plot(1)
toPlot22( c(sLinear) )
toPlot22( c(sLinear) , ranked=FALSE)
toPlot22( c(tPoly, sLinear) )

plot(2)
toPlot22( c(tPoly, NA, 0.5, sLinear) )
plot(3)
toPlot22( c(tPoly, NA, 0.00001, sLinear) ) # because toPlot uses ranks there is no change in colors. but there is a change in the points the algo finds: (nsga2:) with alpha=0.5 the points sample the whole first in-dim, with 0.00001 they are much more specialized and mostly at the 0.1 and 0.9 of the first in-dim.

toPlot22( c(tMulti, hill.size=0, num.minima=100, sLinear) ) # intersting plots
toPlot22( c(tMulti, hill.size=2, num.minima=100, sLinear) )
toPlot22( c(tMulti, hill.size=7, num.minima=100, sLinear) )

toPlot22( c(tNonsep, sLinear) ) # good. can see nonsep.
toPlot22( c(tNonsep, degree=1, sLinear) ) # almost linear
toPlot22( c(tNonsep, degree=10, sLinear) ) # very nonsep.

toPlot22( c(tDecept, deceptive.value=0.003, sLinear) )
toPlot22( c(tDecept, deceptive.value=0.003, aperture=0.1, sLinear) )
toPlot22( c(tDecept, deceptive.value=0.003, aperture=0.25, sLinear) )

toPlot22( c(tSum, sLinear))
toPlot22( c(tNonsep, sLinear))

toPlot22( c(sLinear))
toPlot22( c(sDisc, sDisc))


