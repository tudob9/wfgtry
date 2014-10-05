  # these calculate a long time
source("wfgEvalAndWrap.R")
source("studies.R")
source("withDominated.R")
source("toPlot.R")
source("baGraphics.R")

source("doe.R") # !

# ---- Screening - Plackett-Burman-Design

# install.packages("FrF2")
library(FrF2)
set.seed(1)
pb.plan = pb(12, 7, factor.names = c("alpha", "loc.true.opt", "aperture", "val.decept", "num.minima", "height", "degree") )
pb.plan

set.seed(1)
resp.linear = rep(NA, nrow(pb.plan))
for (r in 1:nrow(pb.plan) ) {
  cat("linear:", r, "of", nrow(pb.plan), "\n")
  resp.linear[r] = wrapListAndAverageHV(sLinear, decode(pb.plan[r, ], 2))
}
mean(resp.linear)
pb.linear = add.response(pb.plan, resp.linear)
pb.linear
screenlinear = lm(pb.linear)
summary(screenlinear)

set.seed(1)
resp.convex = rep(NA, nrow(pb.plan))
for (r in 1:nrow(pb.plan) ) {
  cat("convex:", r, "of", nrow(pb.plan), "\n")
  resp.convex[r] = wrapListAndAverageHV(sConvex, decode(pb.plan[r, ], 2))
}
mean(resp.convex)
pb.convex = add.response(pb.plan, resp.convex)
pb.convex
screenconvex = lm(pb.convex)
summary(screenconvex)

set.seed(1)
resp.concave = rep(NA, nrow(pb.plan))
for (r in 1:nrow(pb.plan) ) {
  cat("concave:", r, "of", nrow(pb.plan), "\n")
  resp.concave[r] = wrapListAndAverageHV(sConcave, decode(pb.plan[r, ], 2))
}
mean(resp.concave)
pb.concave = add.response(pb.plan, resp.concave)
pb.concave
screenconcave = lm(pb.concave)
summary(screenconcave)

summary(screenlinear)
summary(screenconvex)
summary(screenconcave)

# h  d   z_opt  s_ap
# d  h   z_opt  s_ap
# h  d   s_ap   z_opt

# all: loc.true.opt   aperture    height    degree

# ---- modelling - full-factoriel

# install.packages("DoE.base")
library(DoE.base)
set.seed(1)
fd = fac.design(2, 4, factor.names=c("loc.true.opt", "aperture", "height", "degree"))
fd

set.seed(1)
resp.mod.linear = rep(NA, nrow(fd))
for (r in 1:nrow(fd) ) {
  cat("linear:", r, "of", nrow(fd), "\n")
  resp.mod.linear[r] = wrapListAndAverageHV(sLinear, decode(fd[r, ], 2))
}
fd.linear = add.response(fd, resp.mod.linear)
fd.linear
mod.linear = lm(fd.linear)
summary(mod.linear)

# loc.true.opt   height   degree      (aperture:degree)

set.seed(1)
resp.mod.convex = rep(NA, nrow(fd))
for (r in 1:nrow(fd) ) {
  cat("convex:", r, "of", nrow(fd), "\n")
  resp.mod.convex[r] = wrapListAndAverageHV(sConvex, decode(fd[r, ], 2))
}
fd.convex = add.response(fd, resp.mod.convex)
fd.convex
mod.convex = lm(fd.convex)
summary(mod.convex)

# loc.true.opt    height    degree      (aperture1:degree)   (and loc.true.opt1:degree)

set.seed(1)
resp.mod.concave = rep(NA, nrow(fd))
for (r in 1:nrow(fd) ) {
  cat("concave:", r, "of", nrow(fd), "\n")
  resp.mod.concave[r] = wrapListAndAverageHV(sConcave, decode(fd[r, ], 2))
}
fd.concave = add.response(fd, resp.mod.concave)
fd.concave
mod.concave = lm(fd.concave)
summary(mod.concave)

summary(mod.linear)
summary(mod.convex)
summary(mod.concave)

# d,  s_ap:d,  h
# d, s_ap:d, z_opt
# d, h, z_opt:d

# ---- optimization - central composite design

# install.packages("DoE.wrapper")
library(DoE.wrapper)

# separately for the 3 forms
designs = list()

set.seed(1)
designs[[1]] = ccd.design(3, alpha=1, factor.names=list(degree=c(1, 3), aperture=c(1, 3), height=c(1, 3) ) )
set.seed(1)
designs[[2]] = ccd.design(3, alpha=1, factor.names=list(degree=c(1, 3), aperture=c(1, 3), loc.true.opt=c(1, 3) ) )
set.seed(1)
designs[[3]] = ccd.design(3, alpha=1, factor.names=list(degree=c(1, 3), height=c(1, 3), loc.true.opt=c(1, 3) ) )
designs

study = function(design, shape, seed=1) {
  set.seed(seed)
  resp = rep(NA, nrow(design))
  for (r in 1:nrow(design) ) {
    cat(attr(shape, "name"), r, "of", nrow(design), "\n")
    resp[r] = wrapListAndAverageHV(shape, decode(design[r, -1], 3))
  }
  desAndResp = add.response(design, resp)
  return (desAndResp)
}

desAndResp.linear = study(designs[[1]], sLinear) # takes time!
desAndResp.linear
opt.lm.linear = lm(desAndResp.linear)
s.linear = summary(opt.lm.linear)
s.linear

desAndResp.convex = study(designs[[2]], sConvex) # takes time!
desAndResp.convex
opt.lm.convex = lm(desAndResp.convex)
s.convex = summary(opt.lm.convex)
s.convex

desAndResp.concave = study(designs[[3]], sConcave) # takes time!
desAndResp.concave
opt.lm.concave = lm(desAndResp.concave)
s.concave = summary(opt.lm.concave)
s.concave

# linear  h^2, h, d^2
# konvex d, d^2, z_opt^2
# konkav d, d^2, d:h


# ---- prediction:  (here only for linear shape)

nrmse = function(actual, prediction) return ( sqrt((mean( (actual-prediction)^2)))/mean(actual) ) # normalised RMSE

# in-sample

pred.linear = predict(opt.lm.linear)
pred.convex = predict(opt.lm.convex)
pred.concave = predict(opt.lm.concave)

nrmse(desAndResp.linear$resp, s.linear$coefficients["(Intercept)", 1])
nrmse(desAndResp.linear$resp, as.numeric(pred.linear))

nrmse(desAndResp.convex$resp, s.convex$coefficients["(Intercept)", 1])
nrmse(desAndResp.convex$resp, as.numeric(pred.convex))

nrmse(desAndResp.concave$resp, s.concave$coefficients["(Intercept)", 1])
nrmse(desAndResp.concave$resp, as.numeric(pred.concave))

# out-of-sample

  # these 3 take time:
oos.desAndResp.linear = study(designs[[1]], sLinear, seed=2) # 2: out-of-sample
oos.desAndResp.convex = study(designs[[1]], sConvex, seed=2)
oos.desAndResp.concave = study(designs[[1]], sConcave, seed=2)

nrmse(oos.desAndResp.linear$resp, s.linear$coefficients["(Intercept)", 1])
nrmse(oos.desAndResp.linear$resp, as.numeric(pred.linear))

nrmse(oos.desAndResp.convex$resp, s.convex$coefficients["(Intercept)", 1])
nrmse(oos.desAndResp.convex$resp, as.numeric(pred.convex))

nrmse(oos.desAndResp.concave$resp, s.concave$coefficients["(Intercept)", 1])
nrmse(oos.desAndResp.concave$resp, as.numeric(pred.concave))

# (tables)

cbind(desAndResp.linear, pred.linear)
cbind(desAndResp.convex, pred.convex)
cbind(desAndResp.concave, pred.concave)

cbind(oos.desAndResp.linear, pred.linear)
cbind(oos.desAndResp.convex, pred.convex)
cbind(oos.desAndResp.concave, pred.concave)

