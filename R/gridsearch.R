#gridsearch
  # this calculates a very long time. it shows a primitive progress bar

source("studies.R")
source("baGraphics.R")

source("doe.R") # !

# wrapAndAverageHV(sLinear)

height.min = level[["height"]][1]
height.max = level[["height"]][3]
loc.min = level[["loc.true.opt"]][1]
loc.max = level[["loc.true.opt"]][3]

nx = 10
ny = 10
matrices = list()
for(i in 1:3) {
  cat("Matrix", i, "of 3\n")
  d = level[["degree"]][i]
  matrices[[i]] = matrix(NA, nx, ny)
  for(x in 1:nx) for(y in 1:ny) { # dont optimize. 99.99% of the cost is in the 10 randomsearches for each point
    height = x/nx*(height.max-height.min)+height.min
    loc.true.opt = y/ny*(loc.max-loc.min)+loc.min
    set.seed(1)
    matrices[[i]][x, y] = wrapAndAverageHV(sLinear, degree=d, height=height, loc.true.opt=loc.true.opt)
      # height varies in the rows and
      # height varies on the horizontal axis
    cat("matrix", i, ": ", (x-1)*ny+y, "of", nx*ny, "(", x, y, ")\n")
  }
}

require(grDevices)

showGridsearch = function(num) {
  contour(matrices[[num]], xlab="h", ylab="z")
}
showGridsearch(1)
showGridsearch(2)
showGridsearch(3)
baPng("gridsearch1", showGridsearch(1))
baPng("gridsearch2", showGridsearch(2))
baPng("gridsearch3", showGridsearch(3))

