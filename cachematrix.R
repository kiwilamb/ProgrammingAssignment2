## Functions to cache a matrix and its inverse, and to calculate
## an inverse if the inverse doesn't exist in the cache

## Store matrix and inverse in an object

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) i <<- inv
  getInverse <- function() i
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Solve the inverse of a matrix if it doesn't exist in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse()
  inv
}
