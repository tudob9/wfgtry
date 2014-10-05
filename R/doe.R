source("studies.R")
level = list(alpha=c(0.75, 1, 1.25), 
     loc.true.opt=c(0.2, 0.5, 0.8), 
     aperture=c(0.01, 0.05, 0.19), # has to fit with loc.true.opt into the (0, 1) interval
     val.decept=c(0.1, 0.2, 0.5),
     num.minima=c(1, 2, 4),
     height=c(0.1, 0.3, 1),
     degree=c(1, 2, 5)
     )
level
zeroLevels = function() {
  l = list()
  for(i in seq(along=level)) {
    l[[ names(level)[i] ]] = level[[i]][2]
  }
  return (l)
}
wrapSpecific = function(shape, alpha=level[["alpha"]][2],
      loc.true.opt=level[["loc.true.opt"]][2],
      aperture=level[["aperture"]][2],
      val.decept=level[["val.decept"]][2],
      num.minima=level[["num.minima"]][2],
      height=level[["height"]][2],
      degree=level[["degree"]][2]) {
  func = wfgWrap(2, c(
    tPoly, NA, alpha,
    tDecept, NA, loc.true.opt, aperture, val.decept,
    tMulti, NA, num.minima, height, loc.true.opt,
    tNonsep, NA, degree,
    shape
  ))
  return ( func )
}
wrapList = function(shape, list) {
  return ( do.call(wrapSpecific, c(shape, list) ) )
}

wrapListAndAverageHV = function(shape, list) {
  cat("evaluating shape", attr(shape, "name"), "with params:\n\n"); print(list)
  func = wrapList(shape, list)
  return (averagedHV(func, 1000))
}
wrapAndAverageHV = function(shape, ...) {
  cat("evaluating shape", attr(shape, "name"), "with params:\n\n"); print(list)
  func = wrapList(shape, list(...))
  return (averagedHV(func, 1000))
}

decode = function(coded, levels) { # plans have levels 1,2 or 1,2,3. decode them to the actual values
  l = list()
  theNames = names(coded)
  coded = as.numeric(coded)
  for(c in 1:length(coded) ) {
    factorname = theNames[c] 
    encoded = coded[c]
    if(any(encoded<=0)||any(encoded>3)) stop("decode err")
    levelcolumn = encoded
    if(levels==2 && encoded==2) levelcolumn = 3 # in 2 level plans we want the 3'rd column not the 2'nd
    l[[factorname]] = level[[factorname]][levelcolumn]
  }
  return (l)
}
