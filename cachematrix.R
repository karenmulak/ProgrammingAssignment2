## This script contains two functions that calculate the inverse of a matrix, or return the cached
## inverse matrix.
## makeCacheMatrix creates a list of functions for a matrix 'x'.
## cacheSolve uses those functions to check whether the inverse of the matrix has been cached, and 
## otherwise creates and caches the inverse of the matrix.

## Creates a list of functions that refer to a specific matrix 'x'

makeCacheMatrix <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Uses the functions listed in makeCacheMatrix to check to see if the inverse of a matrix has been
## calculated/stored in the cache. If so, it returns that value, otherwise it calculates and returns
## the inverse of the matrix and stores it in the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        
        ## Return a matrix that is the inverse of 'x'
        return(m)
}
