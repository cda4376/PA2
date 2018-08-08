## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Builds a list (object)
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
## x has to be a square matrix, it should not be computationally singular?
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
