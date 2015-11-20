## The first step is assigned a matrix to the variable x:
## x <- matrix(c(...), nrow=?, ncol=?)
## ? number of line and column

## The second step is to assign the function makeCacheMatrix to the object cache:
## cache <- makeCacheMatrix(x)

## This creates a cache object for the inverse matrix.

## cache$set(x)            # Set changes the matrix stored in the main function
## cache$get()             # Get returns the matrix x stored in the main function
## setSolve(x)             # setSolve stores the value of the input in a variable cacheInverse into the main function makeCacheMatrix
## getSolve()              # getSolve returns cacheInverse
## list()                  # list stores the 4 functions described above in the function makeCacheMatrix

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


## The third step is to assign the function cacheSolve to the object cacheSolve:
## cacheSolve(cache)

## This will use makeCacheMatrix() to get the matrix and return the inverse.

## The first thing cacheSolve does is to verify the value cacheInverse, stored previously with getSolve, exists and is not NULL. 
## If it exists in memory, it simply returns a message and the value cacheInverse, that is supposed to be the inverse matrix, but not necessarily.

## If it was the case, "return(cacheInverse)" would have ended the function. So everything that follows this if() is a sort of else {}. 
## data gets the matrix stored with makeCacheMatrix, cacheInverse calculates the inverse of the matrix and x$setSolve(cacheInverse) stores it in the object generated assigned with makeCacheMatrix.

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

## example:
##
## x <- matrix(c(4, 7, 2, 6), nrow=2, ncol=2)
## cache <- makeCacheMatrix(x)      
## cacheSolve(cache)
##
## returns
## [,1] [,2]
## [1,]  0.6 -0.2
## [2,] -0.7  0.4
## 
## when you run a second time you should get a comment "getting cached data"
## cacheSolve(cache)
## getting cached data
## [,1] [,2]
## [1,]  0.6 -0.2
## [2,] -0.7  0.4

## we can also set the matrix as attribute of the function i.e. cache <- makeCacheMatrix(matrix(c(4, 7, 2, 6), nrow=2, ncol=2))
