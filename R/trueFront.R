source("wfgEvalAndWrap.R")
source("wfgSuite.R")

n = 5
M = 2
justConcave = wfgWrap(M, c(sConcave) )
trueConcave = wfgWrap(M, true.front=TRUE, c(sConcave) )
set.seed(1)
jc = plotRS(justConcave, 100)
set.seed(1)
tc = plotRS(trueConcave, 100)

head(jc$value)
head(tc$value)

plot(jc$value)
plot(tc$value)

tmp = randomsearch(justConcave)
plot(tmp$value)

# ---------

compare = function(spec) {
  true = wfgWrap(2, true.front=T, spec)
  random = wfgWrap(2, spec)
  plot(1, main=substitute(spec), sub=substitute(spec))
  plotRS(true, 300)
  plotRS(random, 300)
}
compare(c(sConvex))
compare(c(sConcave))

compare(c(sLinear))
compare(c(sNone, 1, sLinear))
compare(c(sLinear, 1, sNone))

compare(c(sLinear, 1, sMixed)) # this is mixed
compare(c(sMixed, sLinear)) # why is this not mixed?

compare(c(sLinear, 1, sDisc)) # works    (but the dominated points are not removed yet)
compare(c(sDisc, 1, sLinear)) # same

compare(c(sLinear))

compare(c(tPoly, sLinear)) # very hard. default param has too strong influence. almost all points get shifted far away from the true-pf

compare(c(tFlat, sLinear)) # flat visible when points sufficient e.g. plotRS(., 400)

compare(c(tParam, sLinear))

compare(c(sLinear))
compare(c(tLinear, sLinear)) # the same. does default-param of tLin have no influence?

compare(c(tDecept, sLinear)) # no influence?

compare(c(tMulti, sLinear)) # concentration top left.


