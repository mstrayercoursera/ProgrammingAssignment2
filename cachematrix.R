##
## This function creates a special matrix that can cache the given argument.
##
## Returns: a list of functions which get and set the cached matrix and its
##          inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## The cached matrix is stored in m.
  m <- NULL
  
  get <- function() x
  set <- function(y) {
    x <<- y
    
    ## Because the matrix has been 'reset', reset the cached value also.
    m <<- NULL
    ## The effect is to construct a new makeCacheMatrix.
  }
  
  getInverse <- function() m
  setInverse <- function(inv) m <<- inv
  
  list(get = get, set = set, getInverse = getInverse, setInverse = setInverse)
}

##
## The function cacheSolve computes the inverse of the matrix returned by
## makeCacheMatrix above. If this matrix has already been computed, cacheSolve
## returns the cached matrix. The function cacheSolve assumes the matrix x is
## invertible.
##
## Returns: the inverse of matrix x.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'.
  
  ## If a cached matrix exists, return it.
  m <- x$getInverse()
  if (!is.null(m)) {
    message("Using cached matrix...")
    return (m)
  }
  
  ## Otherwise, find the inverse, cache it and return it.
  m <- solve(x$get(), ...)
  x$setInverse (m)
  m
}
