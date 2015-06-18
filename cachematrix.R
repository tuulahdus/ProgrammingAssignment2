## This script provide functions for caching the inverse value of a matrix.
##
## The makeCacheMatrix creates a special "matrix", which is really a list
## containing functions to set/get a matrix, set/get the inverse of a matrix
##
## The cacheSolve calculates the inverse of a matrix created by makeCacheMatrix
## function. However, it first checks to see if the inverse has already been
## calculated. If so, it gets the inverse from the cache and skips the
## computation. Otherwise, it calculates the mean of the data and sets the value
## of the mean in the cache via the setinv function.


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL

  set <- function(y) {
    x <<- y
    i <<- NULL
  }

  get <- function() x

  setinv <- function(inv) i <<- inv

  getinv <- function() i

  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## This function calculates a matrix that is inverse of 'x' (if it's computable) or
## returns precalculated inverse from cache.
cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}