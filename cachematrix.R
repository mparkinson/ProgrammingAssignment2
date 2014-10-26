# The two functions below create a matrix, identify its inverse, store the inverse in the cache, and then make decision about
# whether the cache inverse should be selected or if it should be identified again.

# makeCacheMatrix is used to create a function that builds a matrix.  It sets a randomly named variable (m in this case) to null.
# get, setInverse, and getInverse are all used as objects to store function results.  getInverse and setInverse use the m object,
# which was originally set to null, so that it now contains a value.

makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get, getInverse = getInverse, setInverse = setInverse)
}


# cacheSolve is built around the if function.  If getInverse returns a value other than null, it will return the cache value.
# It is important to remember that m was originally set to null, and was then assigned a value in the makeCacheMatrix function,
# so there will be cases where it is null and others where it contains a value.  If the if function does not return a value,
# a new inverse will be calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    return(m)
  }
  matrixData <- x$get()
  m <- solve(matrixData)
  x$setInverse(m) #Sets the inverse matrix to the inverse of x
  m
}