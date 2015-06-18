## Put comments here that give an overall description of what your
## functions do

## Function takes a matrix as input and stores it providing
## setters and getters to retrieve the inverse of the matrix
## declared as mInverse and returns a special matrix object
makeCacheMatrix <- function(x = matrix()) {
  mInverse <- NULL
  set <- function(y) {
    x <<- y
    mInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) mInverse <<- inv
  getInverse <- function() mInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Function takes the special matrix object and if
## the inverse has not been calculated it finds the
## inverse using the solve function and sets the inverse
## on the special matrix object by calling the setter.
## If the inverse has already been calculated then it is
## returned.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mInverse <- x$getInverse()
  if(!is.null(mInverse)) {
    message("getting cached data")
    return(mInverse)
  }
  mtrx <- x$get()
  mInverse <- solve(mtrx, ...)
  x$setInverse(mInverse)
  mInverse
}
