## The following is a pair of functions that cache 
## the inverse of a matrix.

## The purpose of using these functions is to be able 
## to cache potentially time-consuming computations.

## Function makeCacheMatrix creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  set <- function (y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function () x
  setinv <- function(inv) inverse <<- inv
  getinv <- function () inverse
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}


## Function cacheSolve computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then cacheSolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inverse <- x$getinv ()
  if (!is.null(inverse)){
    message ("getting cached data")
    return (inverse)
  }
  
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinv(inverse)
  return(inverse)
}
