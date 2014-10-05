timing = function(expr, maxSeconds=4) { # returns average evals per second
  begin = proc.time()[1]
  totalEvals = 0
  currentTries = 1
  while (TRUE) {
    currentTime = proc.time()[1]
    if (currentTime > begin+maxSeconds) break;
    if (totalEvals>300 && currentTime-begin > 0.2) break;
    for (i in 1:currentTries) {
      eval(substitute(expr))
    }
    totalEvals = totalEvals + currentTries
    currentTries = ceiling(currentTries*1.2)
  }
  return ( totalEvals / as.numeric(proc.time()[1]-begin) )
}

# usage:
# timing(runif(100*1000))

# some verification:
rand.size = 100*1000
num.evals = timing(runif(rand.size))
num.evals
p = proc.time()[1]
for (i in 1:num.evals) runif(rand.size) 
duration = proc.time()[1] - p # this should approx. 1 sec.
if ( abs(duration - 1) > 0.1 ) stop("timing function inaccurate")

