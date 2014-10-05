# This is a combination of functional tests and tutorial/examples

# tPoly 
#   alpha>0    (default 0.02)
# tDecept
#   optimumLocation    in (0, 1)  (default 0.35)
#   apertureAroundOptimum 0<B<<1      (default 0.001)
#   deceptiveValue    0<C<<1    (default 0.05)
#   (additional constraints)
# tMulti
#   numMinima  natural number    (default 30)
#   hillSize    >0        (default 95.0)
#   optimumLocation  in (0, 1)    (default 0.35)
#   (additional constraints)
# tNonsep
#   degree    natural number  1...|y|  with:  |y| mod degree = 0    

source("wfgEvalAndWrap.R")

# testing:

v2 = c(0.1, 0.2) # example input vectors
v3 = c(0.1, 0.2, 0.3)
v4 = c(0.1, 0.2, 0.3, 0.4)
v5 = c(0.1, 0.2, 0.3, 0.4, 0.5)

# only a shape, no transformations:

shouldValues( wfgEval(v2, 2, c(sLinear)) )
shouldValues( wfgEval(v2, 2, noise=0.01, c(sLinear)) ) # (user should give named args before the spec because the spec can get long)

shouldValues( wfgEval(v3, 2, c(sLinear)) )
shouldValues( wfgEval(v4, 3, c(sLinear)) )
shouldValues( wfgEval(v4, 4, c(sLinear)) )

shouldValues( wfgEval(v5, 2, c(sLinear)) )
shouldValues( wfgEval(v5, 3, c(sLinear)) )
shouldValues( wfgEval(v4, 3, c(sLinear)) )

shouldError(wfgEval, v2, 1, c(sLinear))

shouldError(wfgEval, v2, 2, c(sLinear, sLinear))
shouldValues( wfgEval(v2, 2, c(sLinear, 1, sLinear)) )
shouldValues( wfgEval(v3, 3, c(sLinear, 1, sLinear)) )

shouldValues( wfgEval(v3, 2, c(sLinear, 2)) )
shouldError(wfgEval, v3, 2, c(sLinear, 1.1))

shouldValues( wfgEval(v3, 3, c(sLinear)) )
shouldError(wfgEval, v3, 3, c(sLinear, 2))
shouldValues( wfgEval(v3, 3, c(sLinear, 2, sLinear)) )

shouldValues( wfgEval(v2, 2, c(sLinear, 1, sLinear)) )
shouldValues( wfgEval(v2, 2, c(sLinear, 1, sConvex)) )
shouldValues( wfgEval(v2, 2, c(sConvex, 1, sLinear)) )

shouldValues( wfgEval(v3, 3, c(sLinear)) )
shouldValues( wfgEval(v3, 3, c(sLinear, 1, sLinear)) )

shouldValues( wfgEval(v2, 2, c(sLinear, 1, sMixed)) )

shouldError(wfgEval, v2, 3, c(sLinear, 1, sMixed))
shouldValues( wfgEval(v3, 3, c(sLinear, 2, sMixed)) )
shouldValues( wfgEval(v2, 2, c(sLinear, 1, sMixed)) )
shouldValues( wfgEval(v2, 2, c(sLinear, 1, sDisc)) )
shouldValues( wfgEval(v2, 2, c(sConvex, 1, sMixed)) )
shouldValues( wfgEval(v2, 2, c(sConvex, 1, sMixed)) )
shouldValues( wfgEval(v2, 2, c(sMixed, sDisc)) )

shouldValues( wfgEval(v2, 2, c(sLinear)) )
shouldError(wfgEval, v2, 2, degen=TRUE, c(sLinear))
shouldValues( wfgEval(v3, 3, c(sLinear)) )
shouldValues( wfgEval(v3, 3, degen=TRUE, c(sLinear)) )

for(i in 1:1000) shouldValues( wfgEval(v2, 2, c(sLinear)) )

shouldValues( wfgEval(v2, 2, c(sLinear, 2)) )
shouldError(wfgEval, v2, 2, c(sLinear, 3))
shouldError(wfgEval, v2, 5, c(sLinear))
shouldError(wfgEval, v2, 2, c(sLinear, 1))
shouldError(wfgEval, v2, 2, c(sLinear, 7))
shouldValues( wfgEval(v2, 2, c(sConvex)) )
shouldValues( wfgEval(v2, 2, c(sConcave)) )
shouldError(wfgEval, v2, 1, c(sMixed))
shouldError(wfgEval, v2, 2, c(sMixed))
shouldError(wfgEval, v2, 2, c(sDisc))
shouldValues( wfgEval(v2, 2, c(sMixed, sMixed)) )

# in-order params:

shouldValues( wfgEval(v2, 2, c(sMixed, NA, 0.7, sMixed)) ) # order-based params: NA skips applylength, 0.7 is 'overall'
shouldError(wfgEval, v3, 3, c(sMixed, NA, 0.7, sMixed))
shouldValues( wfgEval(v3, 3, c(sMixed, NA, 0.7, 4, sLinear)) )
shouldValues( wfgEval(v3, 3, c(sDisc, NA, 0.7, 4, 0.5, sLinear)) )

# named params:

shouldValues( wfgEval(v2, 2, c(sMixed, overall=0.7, num=3, sMixed)) ) # ok these two should be equal
shouldValues( wfgEval(v2, 2, c(sMixed, num=3, overall=0.7, sMixed)) )

# order of trafos/shapes is critical, ie these are different:
shouldValues( wfgEval(v4, 4, c(sMixed, overall=0.6, num=3, sMixed, overall=0.4, num=3, sLinear)) )
shouldValues( wfgEval(v4, 4, c(sMixed, overall=0.4, num=3, sMixed, overall=0.6, num=3, sLinear)) )

shouldValues( wfgEval(v2, 2, c(sMixed, overall=0.7, sMixed, overall=0.8)) ) # duplicate param-names are ok

shouldValues( wfgEval(v4, 3, c(sMixed, num=3, overall=0.7, sLinear)) )
shouldValues( wfgEval(v4, 3, degen=TRUE, c(sMixed, num=3, overall=0.7, sLinear)) )

# with transformations:

shouldValues( wfgEval(v3, 3, c(sLinear)) )
shouldValues( wfgEval(v3, 3, c(tPoly, sLinear)) )

shouldValues( wfgEval(v3, 3, c(tPoly, alpha=0.3, sLinear)) )
shouldValues( wfgEval(v3, 3, c(tPoly, alpha=1.4, sLinear)) )
shouldValues( wfgEval(v3, 3, c(tPoly, alpha=1.4, sLinear)) )

shouldValues( wfgEval(v3, 3, c(tPoly, 3, alpha=1.4, sLinear)) )
shouldValues( wfgEval(v3, 3, c(tPoly, 3, 1.4, sLinear)) )
shouldError(wfgEval, v3, 3, c(tPoly, 1, alpha=1.4, sLinear))
shouldValues( wfgEval(v3, 3, c(tPoly, alpha=1.4, tPoly, alpha=1.1, sLinear)) )
shouldValues( wfgEval(v4, 3, c(tPoly, 1, alpha=1.4, tFlat, sLinear)) )

shouldValues( wfgEval(v3, 3, c(tFlat, sLinear)) )
shouldValues( wfgEval(v3, 3, c(tFlat, 3, 0.9, 0.2, 0.5, sLinear)) )
shouldValues( wfgEval(v3, 3, c(tFlat, 3, 0.1, 0.001, 0.999, sLinear)) )

shouldValues( wfgEval(v3, 3, c(tParam, sLinear)) )
shouldValues( wfgEval(v3, 3, c(tParam, factor=0.3, sLinear)) )
shouldValues( wfgEval(v3, 3, c(tParam, NA, 0.05, 0.1, 10, sLinear)) )

shouldValues( wfgEval(v3, 3, c(tLinear, sLinear)) )
shouldValues( wfgEval(v3, 3, c(tLinear, zero.loc=0.7, sLinear)) )

shouldValues( wfgEval(v3, 3, c(tDecept, sLinear)) )
shouldValues( wfgEval(v3, 3, c(tDecept, aper=0.25, sLinear)) ) # much easier to optimize than the default
shouldValues( wfgEval(v3, 3, c(tDecept, dec=0.2, sLinear)) ) # test whether for a given algo the *value* of the deceptive makes a difference
wfgEval(v3, 3, c(tMulti, sLinear))
shouldValues( wfgEval(v3, 3, c(tMulti, NA, 5, 5, sLinear)) ) # much easier to optimize than the default

shouldValues( wfgEval(v3, 3, c(tSum, sLinear)) )
shouldValues( wfgEval(v3, 3, list(tSum, NA, 1, 3, rep(1, 3), sLinear)) ) # with weights-vector. note: need list() to prevent the weights from being flattened
shouldError(wfgEval, v3, 3, list(tSum, NA, 1, 3, rep(1, 2), sLinear))

shouldValues( wfgEval(v3, 2, c(sLinear)) )
shouldValues( wfgEval(v3, 2, c(tNonsep, sLinear)) ) # no difference?
shouldValues( wfgEval(v3, 3, c(sLinear)) )
shouldValues( wfgEval(v3, 3, c(tNonsep, sLinear)) ) # difference. the reason for no difference above is sLinear on 2-dim (the linear p-front is a line of constant value 1 in the first dim, and going from 0 to 2 in the second dim)

shouldValues( wfgEval(v3, 3, c(sLinear)) )
shouldValues( wfgEval(v3, 3, c(tSum, sLinear)) )
shouldValues( wfgEval(v3, 3, c(tSum, 1, tNone, sLinear)) )
shouldValues( wfgEval(v3, 3, c(tSum, 1, tNone, sLinear)) )
shouldValues( wfgEval(v3, 3, list(tSum, from=1, to=3, weights=c(1.5, 1, 0.5), sLinear)) )

shouldValues( wfgEval(v3, 3, c(tParam, sLinear)) )

# like in the param-study:

shouldValues( wfgEval(v3, 3, c(tPoly, tDecept, tMulti, tNonsep, sLinear)) ) # only default values

# in the following only tNonsep's degree is left to the package.
shouldValues( wfgEval(v3, 3, c(tPoly, alpha=0.02, tDecept, NA, 0.35, 0.001, 0.05, tMulti, NA, 30, 95.0, 0.35, tNonsep, sLinear)) )

shouldValues( wfgEval(v3, 2, c(tPoly, alpha=0.02, tDecept, NA, 0.35, 0.001, 0.05, tMulti, NA, 30, 95.0, 0.35,  tNonsep, sLinear)) )

shouldValues( wfgEval(v5, 2, c(tPoly, alpha=0.02, tDecept, NA, 0.35, 0.001, 0.05, tMulti, NA, 30, 95.0, 0.35,  tNonsep, sLinear)) )
shouldValues( wfgEval(runif(5), 2, c(tPoly, alpha=0.02, tDecept, NA, 0.35, 0.001, 0.05, tMulti, NA, 30, 95.0, 0.35,  tNonsep, sLinear)) )

shouldValues( wfgEval(runif(5), 2, c(sLinear)) ) # always 1.0 as first
shouldValues( wfgEval(runif(2), 2, c(sLinear)) ) # always 1.0 as first

shouldValues( wfgEval(runif(5), 2, c(sConvex)) ) # different stuff in first


# get a wrapper function which can be directly passed to algorithms:
f2 = wfgWrap(2, c(sLinear))
shouldValues( f2(v2) )
for(i in 1:1000) f2(v2)
f3 = wfgWrap(3, c(sLinear, 2, sMixed))
shouldValues( f3(v3) )

