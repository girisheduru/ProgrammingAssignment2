## MD 6/29/2016
## Cache the Inverse of a Matrix:
## Inversion is a costly computation that can 
## benefit from caching the matrix rather than computing it over and over.
## the code below create a special object that caches a matrix inverse.


makecachematrix <- function(x = matrix()) {
    invm <- NULL
    set <- function(y) {
        x <<- y
        invm <<- NULL
    }
    get <- function() x
    setinvm <- function(inverse) invm <<- inverse
    getinvm <- function() invm
    list(set = set,
         get = get,
         setinvm = setinvm,
         getinvm = getinvm)
}

## This computes the inverse of the a special "matrix" created by makeCacheMatrix
## If the inverse is calculated then retrieve the Matrix from the cache.

cachesolve <- function(x, ...) {
    
    invm <- x$getinvm()
    if (!is.null(invm)) {
        message("getting cached data")
        return(invm)
    }
    mmat <- x$get()
    invm <- solve(mmat, ...)
    x$setinvm(invm)
    invm
    
}

##Example:
##> source("cachematrix.R")
##> m <- makecachematrix(matrix(1:4, 2, 2))
##> m$get()
##[,1] [,2]
##[1,]    1    3
##[2,]    2    4
##> m$getinvm()
##NULL
##> cachesolve(m)
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> cachesolve(m)
##getting cached data
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
