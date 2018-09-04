## These functions are intended to calculate the inverse of a matrix and to cache 
## the result to avoid reprocessing

## This function saves the provided matrix into the cache and returns a list 
## of functions to save and load the matrix and its inverse from the cache
##
## Args:
##  x: An invertible matrix. Default is the result of matrix()
##
## Returns:
##  A list of four functions:
##      get() : returns the matrix stored into the cache
##      set(x) : saves the matrix into the cache
##      getinverse() : returns the inverse of the matrix (eventually loading it from the cache)
##      setinverse(i) : sets the inverse of the matrix into the cache

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(i) inverse <<- i
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function calculates the inverse of the matrix stored into the cache.
## If the inverse was already calculated it will be read from the cache
##
## Args:
##      x : a list containing the four function returned from makeCacheMatrix
##      ... : further arguments passed to or from other methods
##
## Returns
##  The inverse of the matrix stored into the cache

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("Getting cached data")
        return(inverse)
    }
    matrix <- x$get()
    inverse <- solve(matrix, ...)
    x$setinverse(inverse)
    inverse
}
