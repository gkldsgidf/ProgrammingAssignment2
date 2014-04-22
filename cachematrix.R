## My functions create the inverse of a given matrix and stores it to 
## avoid wasting time and resources. They are specially created to deal with big matrices, 
## i.e. matrices with dozens, hundreds or even thousands of rows and columns.

## The makeCacheMatrix function sets and gets the matrix, and sets and gets the inverse 
## matrix. This is done by creating a special matrix which can be cached its inverse.

makeCacheMatrix <- function(x = matrix()) {
  t <- NULL
  set <- function(y) {
    x <<- y
    t <<- NULL
  }
  get <- function(x)
  setInverse <- function(solve(t)) <<- solve
  getInverse <- function(t)
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## The cacheSolve function checks if the inverse matrix has been calculated by
## the makeCacheMatrix function. If yes, it gets the inverse matrix from the cache. 
## If not, it works out the inverse matrix and sets it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  cacheInverse <- function(x, ...) {
    t <- x$getInverse()
    if(!is.null(t)) {
      message("Getting cached data")
      return(t)
    } else {
    data <- x$get()
    t <- solve(data, ...)
    x$setInverse(t)
    return(t)
    }
  }
}
