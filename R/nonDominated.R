# non dominated points (new)
# similar in function to emoa::nonDominated_points() but this retains extra columns. 
# this allows us to know the search-space coordinates. 
# this is similar to mco::paretoSet() however that depends on the optimization-algorithm being able to calculate the logical 'dominated' column itself. and this does not have any dependencies

nonDominated = function(matr, numCoords) { # the first columns are coordinates, the remaining their values (on which to determine which are nonDom)
  numValues = ncol(matr)-numCoords
  l = list()
  for (i in 1:numValues) l[[i]] = matr[, numCoords+i]
  ord = do.call(order, l)
  D = matr[ord, ]
  nonDom = D
  for (i in 1:(numValues-1) ) {
    nonDom = nonDom[which(!duplicated(cummin(nonDom[, numCoords+i+1]))), ]
  }
  return (nonDom)
}

# example: this shows two values on the two axis' and their two search-space coordinates as color and form
# value1 = runif(20)
# value2 = runif(20)
# coord1 = 1:20
# coord2 = 20:1
# d = cbind(coord1, coord2, value1, value2)
# d
# nonDom = nonDominated(d, 2)
# nonDom
# plot(d[,3:4], col=d[, 1], pch=d[, 2], xlim=c(0,1), ylim=c(0,1))
# plot(nonDom[,3:4], col=nonDom[, 1], pch=nonDom[, 2], xlim=c(0,1), ylim=c(0,1))
