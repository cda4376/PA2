## Put comments here that give an overall description of what your
## functions do.
## The functions are supposed to be called like 
## cacheSolve(makeCacheMatrix(x))) where x is a square matrix.
## x should not be computationally singular?
## e.g cacheSolve(makeCacheMatrix(matrix(c(1:9), 3, 3))) triggers:
##  Error in solve.default(data) : 
##    Lapack routine dgesv: system is exactly singular: U[3,3] = 0

## Write a short comment describing this function
## Builds a list (object) of object methods(?).
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(loc_inv) inv <<- loc_inv
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Write a short comment describing this function
## Return inverse of x
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  loc_x <- x$get()
  inv <- solve(loc_x, ...)
  x$setInverse(inv)
  inv
}
