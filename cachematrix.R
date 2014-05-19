## This module provides two functions which leverage caching to speed up
## repeated calculation of the inverse of a matrix which is not changed
## in between the calculations.
##
## Usage:
## m <- generateMatrix()    # Assuming that a square invertible matrix is
##                          # generated
## cm <- makeCacheMatrix(m) # wrap the matrix with the helper functions
## cacheSolve(cm)           # First call, inverse calculated
## cacheSolve(cm)           # Second call, no change, cached result returned

## This function wraps a plain matrix in a list with some additional
## helper function to enable caching of the inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function calculates the inverse of the matrix, or returns the cached
## results if it is called multiple times for an unchanged matrix.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(is.null(i)) {
        ## NOTE: The example code checked for !is.null(i) and would return the
        ##       cached results inside this if. I dislike double negations. They
        ##       are harder to read and require more effort to understand. So, I
        ##       calculate the results inside this if statement.
        i <- solve(x$get(), ...)
        x$setinverse(i)
    }
    i
}
