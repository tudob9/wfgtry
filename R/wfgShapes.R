# Shapes:
# the function-names are those in the wfg-paper plus prefix wfg
# in the spec the user writes it without wfg:
#
# conventions used:
# *here* ie in individual shapes: x always has dim M-1 (not M). result is always M-dim.

source("wfgUtil.R")

wfgsNone = function(y) { return (invisible(NULL)) } # just proxy
attr(wfgsNone, "type") = "wfgShape"
attr(wfgsNone, "name") = "wfgsNone"
sNone = wfgsNone # important that this is after the attributes

wfgLinear = function(x) {
  M = length(x)+1
  res = rep(NA, M)
  res[1] = prod(x)
  if (2<=M-1) for(m in 2:(M-1)) res[m] = prod(x[1:(M-m)]) * (1-x[M-m+1])
  res[M] = 1-x[1]
  return(to01(res))
}
attr(wfgLinear, "type") = "wfgShape"
attr(wfgLinear, "name") = "wfgLinear"
sLinear = wfgLinear

wfgConvex = function(x) {
  M = length(x)+1
  res = rep(NA, M)
  res[1] = prod(1 - cos(x*pi/2))
  if (2<=M-1) for(m in 2:(M-1)) res[m] = prod( 1-cos( x[1:(M-m)]*pi/2) ) * (1 - sin( x[M-m+1]*pi/2 ) )
  res[M] = 1 - sin(x[1]*pi/2)
  return(to01(res))
}
attr(wfgConvex, "type") = "wfgShape"
attr(wfgConvex, "name") = "wfgConvex"
sConvex = wfgConvex

wfgConcave = function(x) {
  M = length(x)+1
  res = rep(NA, M)
  res[1] = prod(sin(x*pi/2))
  if (2<=M-1) for(m in 2:(M-1)) res[m] = prod( sin( x[1:(M-m)]*pi/2) ) * cos( x[M-m+1]*pi/2 )
  res[M] = cos(x[1]*pi/2)
  return(to01(res))
}
attr(wfgConcave, "type") = "wfgShape"
attr(wfgConcave, "name") = "wfgConcave"
sConcave = wfgConcave

wfgMixed = function(x, overall=1.0, num=2) { # overall>1~<1 => convex~concave. num is number of convex/concave regions
  if (num%%1!=0) stop("number of convex/concave regions should be an integer")
  # only uses x[1] think correct because in 'disconnected' for beta they state x[1] explicitly
  if (overall<=0) stop("overall shape has to be >0")
  if (num<=0 | floor(num)!=num) stop("number of convex+concave parts has to be a natural number")
  if (wfg.verbose) cat("mixed with", overall, "and", num, "\n")
  A = num # assignments like these are to match the wfg-paper (while still having descriptive parameter-names in R)
  alpha = overall
  temp = 2*A*pi
  res = (1-x[1]- cos(temp*x[1] + pi/2)/temp) ^ alpha
  return(to01(res))
}
attr(wfgMixed, "type") = "wfgShape"
attr(wfgMixed, "name") = "wfgMixed"
sMixed = wfgMixed

wfgDisc = function(x, overall=1.0, num.regions=2, location=1.0) {  #!!! overall>1~<1 => concave~convex. the opposite of the above! acc. to paper. paper correct?
  if (overall<=0) stop("overall shape has to be >0")
  if (location<=0) stop("location has to be >0")
  if (num.regions<=0 | floor(num.regions)!=num.regions) stop ("number of disconnected regions has to be a natural number")
  A = num.regions
  alpha = overall
  beta = location
  res = 1 - x[1]^alpha * cos( A * x[1]^beta * pi )^2 
  return(to01(res))
}
attr(wfgDisc, "type") = "wfgShape"
attr(wfgDisc, "name") = "wfgDisc"
sDisc = wfgDisc

