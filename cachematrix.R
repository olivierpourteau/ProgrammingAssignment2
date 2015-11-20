## These functions retuns the inverse of a matrice

## makeCacheMatrix creates a cache object for the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        
        cacheInverse <- NULL
        set <- function(y) {
                x <<- y
                cacheInverse <<- NULL
        }
        get <- function() x
        setSolve <- function(solve) cacheInverse <<- solve
        getSolve <- function() cacheInverse
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
        
}

## cacheSolve will use makeCacheMatrix() to get the matrix and return the inverse.

cacheSolve <- function(x, ...) {
        cacheInverse <- x$getSolve()
        if(!is.null(cacheInverse)) {
                message("getting cached data")
                return(cacheInverse)
        }
        data <- x$get()
        cacheInverse <- solve(data, ...)
        x$setSolve(cacheInverse)
        cacheInverse
}
