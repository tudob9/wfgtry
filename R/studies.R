# These are parameter-studies
# source("studies.R")

# wfgExperiment is at the bottom

# [0, 1]^5 -> R^2
# shapes: linear, convex, concave
# always these transformations in this order: (also given are the parameters)
# b_poly: bias alpha
# s_decept: location A, aperturesize B, value C
# wfgs_multi: optimumLocation A, apertureAroundOptimum B (C is set equal to decept's A)
# nonsep: degreeOfNonsep A

source("wfgEvalAndWrap.R")
source("randomsearch.R")
  shapedefault = sConvex

# for nsga2:
# install.packages("mco")
library(mco) # paretoFilter

# install.packages("devtools")
library(devtools)
# devtools::install_github("BBmisc", username="berndbischl")
library(BBmisc)
# devtools::install_github("ParamHelpers", username="berndbischl")
library(ParamHelpers)
# install.packages("emoa")
library(emoa) # dominated_hypervolume

# wfgStudy
  #..todo. in the following tNonsep's degree is still left to the package.

wrapNsga2 = function(func, numEvals=1000) {
  # divide numEvals into popsize*generations:
  popsize = floor(sqrt(numEvals)/4)*4
  generations = floor(numEvals/popsize)
  return ( nsga2(func, 5, 2, lower.bounds=rep(0, 5), upper.bounds=rep(1, 5), popsize=popsize, generations=generations) )
}

wfgFilter = function(mco) { # mco-obj -> mco-obj . only keep the non-dominated points
  
}
plotRS = function(func, numRandomSearchPoints=1000) {
  tmp = randomsearch(func, numRandomSearchPoints)
  plot(tmp$value)
  return (tmp)
}
plotIndivsInObjSpace = function(spec) {
  plotRS(wfgWrap(2, spec))
}

wfgHV = function(points) { # points contains points in rows
  ref = c(3, 5)
  if(any(points[, 1]>3) || any(points[, 2]>5) ) stop("reference-point too small")
  outDim = 2
  if(ncol(points) != outDim) stop(paste("vector-dim and dim of ref-point should be the same"))
  # dominated_hypervolume requires them in columns:
  res = dominated_hypervolume(t(points), ref) # the reason for 3,5 being enough is the default scaling-factors S of 2,4 (note that this studies-function depends on that default not being changed, otherwise it shows a good error message)
  return(res)
}
averagedHV = function(func, numRandomSearchPoints=1000, algo=randomsearch) {
  runs = 10
  HVs = rep(NA, runs)
  for(r in 1:runs) {
    mco = algo(func, numRandomSearchPoints)
    # mco = nsga2(func, 5, 2, lower.bounds=rep(0, 5), upper.bounds=rep(1, 5), generations=5)
    # wfgPPlot(paretoFilter(mco$value))
    
    # filtered = paretoFilter(mco$value)
    # filtered = mco$value
    
    # plot(mco$value)   # keep this. e.g. some interpretations in paramSlide.R
    
    thisHV = wfgHV(mco$value)
    HVs[r] = thisHV
    # cat("thisHV: ", thisHV, "\t\tmean ", mean(HVs, na.rm=TRUE), "\t\tsd ", sd(HVs, na.rm=TRUE), "\n")
  }
  return (mean(HVs))
}

nonsep_degreeDefault = 1  # todo: make variable

# paramset design contents -> vector like the functions need them. the optimumLocation is taken twice
extractFromDesign = function(des, i) { # i'th row is i'th paramComb
  return ( c(des[i, "poly_alpha"], 
            des[i, "optimumLocation"], 
            des[i, "decept_apertureAroundOptimum"], 
            des[i, "decept_deceptiveValue"], 
            des[i, "multi_numMinima"], 
            des[i, "multi_hillsize"], 
            des[i, "optimumLocation"],
            nonsep_degreeDefault
          ) )
}

  # wfgParams: in this study the user specifies how many parameter combinations should be evaluated
  # the parameters are taken from a search-space. the search-space has useful defaults, the user can change some or all of them.
wfgParams = function(numParamCombis=20, polyAlphaLow=0.0001, polyAlphaHigh=2.0, optiLocLow=0.001, optiLocHigh=0.9999, apertureLow=0.0001, apertureHigh=0.25, deceptValueLow=0.01, deceptValueHigh=0.25, numMinimaLow=1, numMinimaHigh=100, hillsizeLow=0.01, hillsizeHigh=2.0, nonsepLow=0, nonsepHigh=5) {
  numRandomSearchPoints = 1000
  
  ps = makeParamSet(
    makeNumericParam("poly_alpha", lower = polyAlphaLow, upper = polyAlphaHigh),
    makeNumericParam("optimumLocation", lower = optiLocLow, upper = optiLocHigh), # for both decept and multi
    makeNumericParam("decept_apertureAroundOptimum", lower = apertureLow, upper = apertureHigh),
    makeNumericParam("decept_deceptiveValue", lower = deceptValueLow, upper = deceptValueHigh),
    makeIntegerParam("multi_numMinima", lower = numMinimaLow, upper = numMinimaHigh),
    makeNumericParam("multi_hillsize", lower = hillsizeLow, upper = hillsizeHigh),   # (4*numMinima+2)*pi >= 4*hillsize
    makeIntegerParam("nonsep_degree", lower = nonsepLow, upper = nonsepHigh) # todo
  )

  des = generateDesign(numParamCombis, ps)
  des[, ncol(des)+1] = rep(NA, numParamCombis) # reserve for average achieved HV
  names(des)[ncol(des)] = "av.HV"
  
  for(i in 1:numParamCombis) {
    cat("i ", i, "\n")
    params = extractFromDesign(des, i)
    cat("extr: \n")
    print(params)

    hypervolume = wrapAndAverageHV(numRandomSearchPoints, params)
    
    cat("pc")
    str(des[i, ])
    cat("average achieved HV:", hypervolume, "\n")
    des[i, ncol(des)] = hypervolume
  }
  return(des)
}
# wfgParams(numParamCombis=2)
  # shapedefault = sConcave

# ---

wfgShapeTest = function() {
  numRandomSearchPoints = 1000
  
  points = 1
  m = matrix(NA, points, 2)
  for(i in 1:points) {
    # func = wfgWrap(2, c( sConvex ))
    # func = wfgWrap(2, c( sConcave ))
    func = wfgWrap(2, c( sNone ))
    hv = averagedHV(func, numRandomSearchPoints)
  }
  
  return (invisible(NULL))
}
# wfgShapeTest()

# ---
wfgParam = function(numParamCombis=20, varying="optiLoc") {
  # normally you should try to use legal parameter-combinations! but this function shows a practical way to select parameters independently and ignore those that are illegal (just trying next; try until the asked number of combinations was gathered)

    # optiLoc polyAlpha aperture deceptValue numMinima hillsize nonsep
  numRandomSearchPoints = 1000

  paramMins = c(optiLoc=0.001, polyAlpha=0.0001, aperture=0.0001, deceptValue=0.01, numMinima=1, hillsize=0.01, nonsep=0)
  paramMaxs = c(optiLoc=0.9999, polyAlpha=2.0, aperture=0.25, deceptValue=0.25, numMinima=100, hillsize=2.0, nonsep=5)
  params = c(optiLoc=runif(1, paramMins[["optiLoc"]], paramMaxs[["optiLoc"]]),
        polyAlpha=runif(1, paramMins[["polyAlpha"]], paramMaxs[["polyAlpha"]]),
        aperture=runif(1, paramMins[["aperture"]], paramMaxs[["aperture"]]),
        deceptValue=runif(1, paramMins[["deceptValue"]], paramMaxs[["deceptValue"]]),
        numMinima=round(runif(1, paramMins[["numMinima"]], paramMaxs[["numMinima"]])),
        hillsize=runif(1, paramMins[["hillsize"]], paramMaxs[["hillsize"]]),
        nonsep=round(runif(1, paramMins[["nonsep"]], paramMaxs[["nonsep"]])))
  if(is.null(params[[varying]])) stop("varying parameter unknown")

  cat("fixed params chosen as:\n")
  print(params)
  cat("varying: ", varying, ":\n")
  
  m = matrix(NA, numParamCombis, 2)

  # varies = seq(length.out=numParamCombis, paramMins[[varying]]+0.1, paramMaxs[[varying]]-0.1) # 0.1 todo

  current = 1
  for(tries in 1:(numParamCombis*5) ) { # 5 heuristic
    
    # params[[varying]] = varies[current]
    params[[varying]] = runif(1, paramMins[[varying]], paramMaxs[[varying]])

    cat("current ", current, "\n")
    hypervolume = NA # scope
    try( hypervolume <- wrapAndAverageHV(numRandomSearchPoints, c(
      params[["polyAlpha"]],
      params[["optiLoc"]],
      params[["aperture"]],
      params[["deceptValue"]],
      params[["numMinima"]],
      params[["hillsize"]],
      params[["optiLoc"]],
      nonsep_degreeDefault
    ) ), silent=TRUE )

    cat("average achieved HV:", hypervolume, "\n")
    m[current, ] = c(params[[varying]], hypervolume)
    if(!is.na(hypervolume)) {
      current = current + 1
    }
    if(current==numParamCombis+1) break
  }
  if(current<numParamCombis) stop("invalid parameters")
  cat("result:\n")
  print(m)
  return(m)
}

# res = wfgParam(10)
# res = wfgParam(10, varying="testing")
# res = wfgParam(10, varying="aperture")
# res
# res = res[order(res[,1]), ]
# res
# plot(res)
