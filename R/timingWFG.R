source("timing.R")
source("wfgSuite.R")

# WFG:
# (each of the following applies all trafos/shapes to all entries of the 5->2 problem)
spec1 = c(tMulti, sConcave)
f1 = wfgWrap(2, spec1 )
timing(f1(rep(0.1, 5)))
timing(wfgEval(rep(0.1, 5), 2, spec1 ) )

spec4 = c(tPoly, tDecept, tMulti, tNonsep, sConvex)
f4 = wfgWrap(2, spec4 )
timing(f4(rep(0.1, 5)))
timing(wfgEval(rep(0.1, 5), 2, spec4 ) )

timing( wfg1(rep(0.1, 5)) )

# runif vs maximin-lhs:

ps = makeParamSet(
makeNumericParam("x1", lower = 0.0, upper = 1.0),
makeNumericParam("x2", lower = 0.0, upper = 1.0),
makeNumericParam("x3", lower = 0.0, upper = 1.0),
makeNumericParam("x4", lower = 0.0, upper = 1.0),
makeNumericParam("x5", lower = 0.0, upper = 1.0)
)

# can have 100 times the points if not doing LHS  (note randomLHS() does not LHS)
timing(generateDesign(1000, ps, fun=maximinLHS))
timing(generateDesign(100000, ps))

# maximinLHS has quadratic runtime!:
m = matrix(NA, nrow=10, ncol=2)
for(i in 1:10) {
  pt = proc.time()[1]
  a = generateDesign(2**i, ps, fun=maximinLHS) # improvedLHS has same form
  m[i, ] = c(2**i, proc.time()[1] - pt)
}
plot(m)
