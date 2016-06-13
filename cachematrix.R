## Copyright Jeffrey Yu, 2016

## Utility functions for operations on CacheMatrix, a type of matrix which
## can store cached values instead of recalculating them every time.

## Returns a matrix that caches its inverse value and resets it whenever the
## matrix values are changed.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
          x <<- y
          inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Returns the inverse of a CacheMatrix. If the inverse has already been
## calculated, returned the cached value instead of calculating it again.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("Getting cached inverse")
    return(inv)
  }
  matr <- x$get()
  inv <- solve(matr, ...)
  x$setInverse(inv)
  inv
}
