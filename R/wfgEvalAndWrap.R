source("wfgShape.R")
source("wfgTransformation.R")

checkTrafoComplete = function(target.index) {
  if (target.index!=1) {
    if (wfg.verbose) cat("should be 1: ", target.index, "\n")
    stop("the last transformation specification should either omit the applylength (default is apply-to-rest) or be consistent with the number of remaining entries of the target")
  }
  return (invisible(NULL))
}
checkShapeComplete = function(target.index, current.shape) {
  if (target.index!=1) {
    if (wfg.verbose) cat("should be 1: ", target.index, "\n")
    if ( identical(current.shape, wfgMixed) || identical(current.shape, wfgDisc) ) {
      stop("mixed and disconnected shapes always have applylength 1 but then this specification does not cover the remaining entries of the target")
    } else {
      stop("the last shape specification should either omit the applylength (default is apply-to-rest) or be consistent with the number of remaining entries of the target")
    }
  }
  return (invisible(NULL))
}
tToX = function(t, M, A) { # the t-to-x transition from the wfg-paper. this is applied after all transformations before all shapes
    # A: vector of {0, 1} degeneracy constants. for each that is 0 the p-front will have one less dimension
  # calc x from t:
  x = rep(NA, M)
  if (1<=M-1) for (i in 1:(M-1)) {
    x[i] = max(t[M], A[i])*(t[i]-0.5)+0.5
  }
  x[M]=t[M]
  return (x)
}
wfgEval = function(z, num.objectives, spec, z.maxes=NA, num.pos.rel=NA, degen=FALSE, scales=NA, dist=NA, noise=0.0, true.front=FALSE) {

  if (wfg.verbose) { 
    cat("wfgEval at:\n")
    str(z)
    cat("evaluating: \n")
    print(spec)
  }
  # the spec-evaluator
  #
  # todo?: this could be factored into 3 methods: prep, loop, composition
  #        the loop itself could be made into: prep, trafo, shape
  #        but all of them modify multiple variables so it would require a lot of return-vector unpacking
  #
  # z: is to be evaluated
  # z.maxes: 0<=z<=z.maxes . default is 1 for each dim
  # num.pos.rel: number of position-related parameters
  # num.objectives: number of objectives
  # degen: whether to degenerate the dimension (after all trafos, before all shapes)
  # scales: a vector of M scaling-factors for the result of the shape (in wfg-paper: S)
  # dist: a factor for the distance from the p-front.
  # noise: standard-deviation of multivariate normal distribution with mean 0.0
  # spec: a vector containing any number of times: name of a trafo-function, applylength, followed by any number of 
  #       numeric params that will be passed to the trafo-func
  #     followed by any number of times: the same for shape-functions
  #    in both: the params can be named or ordered see examples

  n = length(z)
  M = num.objectives # assignments like these are to match the wfg-paper (while still having descriptive parameter-names in R)
  if (M<1) stop("number of objectives should be positive")
  if (M==1) stop("number of objectives (M) should be greater than 1 (because number of position-related inputs has to be divisible by M-1)")
  if (n<M) stop("number of objectives cannot be larger than input dimesion")

  S=scales
  if (is.na(S)) S = (1:M)*2 # scaling constants. this default taken from wfg c++ code
  D = dist
  if (is.na(D)) D = 1 # distance scaling constant. paper says >0 and =1 in [43](wfg2005).

  k = num.pos.rel 
  if (is.na(k)) {
    # the number of position-related inputs must obey constraints.
    # this finds a useful default for k such that l (num distance-related inputs) is minimal but not 0; 1 would be optimal.
    if (1<=n-1) for (kTry in 1:(n-1)) {
      if (kTry%%(M-1)==0) {
        k = kTry # overwrite previous best
      }
    }
    if (is.na(k)) stop("no best constraint-respecting default for k (number of position-related inputs) could be found")
    if (wfg.verbose) cat("best constraint-respecting default generated for k (number of position-related inputs) is ", k, "\n")
  }
  l = n-k # wfg: number of distance-related inputs (wfg: "any positive integer". but should n=k+l with k divisible by M-1)
  if (k%%(M-1)!=0) stop("number of position-related inputs k has to be divisible by M-1 (M number of objectives)")
  # check k: (especially if we make it variable to the user)
  if (k<=0) stop("number of position-related inputs cannot be negative")

  # check l (if no-longer hardcoded):
  if (l<=0) stop("number of distance-related inputs l must be a positive integer")
  
  for (i in 1:n) {
    if (is.na(z.maxes[i])) z.maxes[i] = 1
    # note: this works also for z.maxes=NA not only for z.maxes=c(NA)
  }
  if (length(z) != length(z.maxes)) stop("z and z.maxes should have same number of entries")
  if (any(z<0)) stop("all entries of z should be non-negative")
  if (any(z>z.maxes)) stop("each entry of z should be less than or equal to the corresponding z.maxes entry")
  if (M>n) stop("number of objectives may not be larger than the dimension of the search-space (the size of z)")
  if (noise<0.0) stop("noise must be non-negative (standard-deviation of multivariate normal distribution)")

  z01 = z/z.maxes
  ts = z01 # will successively hold all the t1->...->tp

  lenSpec = length(spec)
  if (lenSpec==0) stop("empty specification") # return (z)
  #for (i in 1:lenSpec) {
  #  spec[[i]] = eval(parse(text=spec[[i]]))
  #}
  
  # loop preparation:
  iSpec = 1 # index of current word in the specification
  # in the following: applylength is the first number after the transformation-name
  target.index = 1 # index of the current first-vector-entry. 
    # starting with target.index and going for applylength entries will be transformed
    # will be increased by applylength
  still.trafos = TRUE
  x = NA # scope
  shape.applied = NA # scope
  if (wfg.verbose) cat("m: before first:", ts, "\n")

  while(TRUE) { # goes through the spec. evaluates any transformations then all shapes
    #** the first blocks (upto next #** mark) are used for both trafo and shape

    if (wfg.verbose) cat("---- spec at pos:", iSpec, "----\n")
    if (iSpec>lenSpec) {
      if (wfg.verbose) cat("spec done\n")
      checkShapeComplete(target.index, current)
      break
    }
    
    current = spec[[iSpec]]
    # check that current is a function not a number:
    if (!is.function(current)) stop("at this point in the specification should be a trafo/shape-name")
    params = parseParams(spec, iSpec)
    paramsOrigLength = length(params) # remember to know how many to skip
    
    applylength = NA
    # rule: if the first parameter is not *named* (e.g. does not have a name like "alpha=...") then it is assumed to be applylength. if you want the default applylength and additionally non-named ('positional') non-default parameters then use NA as the first parameter.
    if (length(params)>0 && (is.null(names(params)) || names(params)[1]=="") ) {
      applylength = params[[1]] # *may* also be an explicit NA
      # remove applylength
      params[[1]] = NULL
    }
    if (is.na(applylength)) { # handle the case when spec does not contain an applylength:
      # apply to the remaining entries (possibly all)
      # special case: mixed and disc shapes have to have applylength 1
      
      if ( identical(current, wfgMixed) || identical(current, wfgDisc) ) { # compat?
        applylength = 1
      } else { # not the special case. all remaining entries
        applylength = M-target.index+1
        if (wfg.verbose) cat("applying ", attr(current, "name"), " to all remaining ", applylength, "\n")
      }
    } else {
      if ( !is.numeric(applylength) || !(length(applylength)==1) || applylength%%1 != 0) { # not is.integer
        stop("if there are arguments and the first argument is not named then it should be either the integer applylength (how many entries of the vector this transformation is applied to) or NA (resulting in the default 'all-remaining')")
      }
      # only check the special case
      if ( (identical(current, wfgMixed) || identical(current, wfgDisc)) && applylength!=1 ) stop("mixed or disconnected shapes can only have applylength 1 (if omited then 1 is the default)")
      if (target.index+applylength-1 > n) stop(paste("too large applylength (=", applylength, "): cannot apply beyond target's size (wrap has to be done explicitly to avoid errors)  (t.i. was ", target.index))
      if (wfg.verbose) cat("applying to ", applylength, "\n")
    }

    #** now depending on whether trafo or shape:
    if (still.trafos) { # do transformations
      # want to get the user to first spec all trafo then all shapes, so we dont decide on-demand but switch at a certain point.
      if (attr(current, "type")!="wfgTrafo") { # a shape was encountered. this does the transition and switches to shapes. # FIXME: this should be the else-case.
        if (wfg.verbose) cat("non-trafo\n")
        checkTrafoComplete(target.index)
        still.trafos = FALSE
        
        if (degen) {
          if (wfg.verbose) cat("degenerating\n")
          if (M-2<=0) stop("not enough objectives to degenerate")
          A = c(1, rep(0, M-2))
        } else {
          if (wfg.verbose) cat("not degenerating\n")
          A = rep(1, M-1) 
        }

        if (true.front) { # true-Pareto-front overrides. (note: we discard the work the transitions did; transitions *could* be skipped altogether, but we dont want the error-behavior (spec checks) to depend on whether true.front is true/false)
          ts = z01 # discard transitions' work
          if (wfg.verbose) cat("true front\n")
        }

        x = tToX(ts, M, A) # the transition

        if (true.front) x[M] = 0

        if (wfg.verbose) cat("t to x: ", ts, x, "\n")

        # switch to shapes
        ts = NA # "dont use"
      } else { # do trafo
        if (wfg.verbose) cat("trafo with params:\n")
        if (wfg.verbose) str(params)
        tsNew = wfgTransformation(M, current, ts, target.index, applylength, n, k, params)

        if (wfg.verbose) cat("tNew: ", tsNew, "\n")

        iSpec = iSpec + 1 + paramsOrigLength
        ts = tsNew
        target.index = target.index + applylength
        if (target.index>M) target.index = 1
      }
    }
    if (!still.trafos) { # do shape. should not be the 'else' to the previous 'if' because then the top of the loop would re-execute before handling the first shape
      if (wfg.verbose) cat("shape ", attr(current, "name"), " with\n")
      if (wfg.verbose) str(params)
      iSpec = iSpec + 1 + paramsOrigLength
      if (!identical(current, wfgsNone)) {
        if (wfg.verbose) cat("before shape: ", x, "\n")
        shape.applied = wfgShape(x, current, target.index, applylength, params)
        if (wfg.verbose) cat("after shape: ", shape.applied, "\n")
      } else {
        if (wfg.verbose) cat("sNone\n")
      }
      if (target.index+applylength>M+1) stop("the combination of index", target.index, "and applylength", applylength, "does not fit into the available target-dim. of", m)
      if (target.index+applylength==M+1) {
        if (iSpec>lenSpec) {
          target.index = 1
          break; # done with the shape
        }
        stop("the shapes have completed for every target-entry but found additional entries in the spec")
      }
      target.index = target.index + applylength
      # wrap-around (now prevented above)   if (target.index>M) target.index = 1
    }
    
  }

  #** post-loop operations
  
  if (still.trafos) stop("specification should contain at least one shape")
  
  # finally combination with last entry:
  f = rep(NA, M)
  
  if (wfg.verbose) cat("finishing ", D, S, "X: ", x, "SA: ", shape.applied, "\n")

  if (1<=M) for (i in 1:M) {
    f[i] = D*x[M]+S[i]*shape.applied[i]
    if (wfg.verbose) cat("finish ", i, ": ", f, "\n")
  }

  if (noise!=0.0) f = f + rnorm(M, 0.0, noise)
  return (f)
}
wfgWrap = function(num.objectives, spec, z.maxes=NA, num.pos.rel=NA, degen=FALSE, scales=NA, dist=NA, noise=0.0, true.front=FALSE) { 
  # to get a wrapper function which can be directly passed to algorithms
  f = function(z) {
    res = do.call(wfgEval, list(z, num.objectives, spec, z.maxes, num.pos.rel, degen, scales, dist, noise, true.front))
    return (res)
  }
  return (f)
}
