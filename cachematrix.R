## This program defines two functions to get the inverse of a matrix
## and cache the results so that it can be referenced repeatedly without
## having to repeate potentially costly calculations.
##
## For "Programming R" on Coursera
##
## Example usage:
##   m  <- matrix(1:4,2)
##   cm <- makeCacheMatrix(m)
##   cacheSolve(cm)



## Returns a new object containing 'x' that has methods for caching
## the inverse of 'x'

makeCacheMatrix <- function(x = matrix()) {
    # Contains the cached inverse of the matrix 'x'
    cachedinv <- NULL

    # Sets/gets the value of 'x' and clears the cache if necessary
    set <- function(y) {
        x <<- y
        cachedinv <<- NULL
    }
    get <- function() x

    # Sets/gets the cached inverse
    setinverse <- function(inverse) cachedinv <<- inverse
    getinverse <- function() cachedinv

    # Return a new object with the appropriate methods
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Invert a matrix returned from 'makeCacheMatrix' or use the cache if available

cacheSolve <- function(x, ...) {
    # First attempt to get the cached solution
    inverse <- x$getinverse()

    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }

    # Otherwise solve and cache the results
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
