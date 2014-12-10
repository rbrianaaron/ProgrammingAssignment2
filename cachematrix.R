## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse
## of a matrix rather than computing it repeatedly.This pair of functions cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        #This function provides the structure to set and get a makeCacheMatrix object. 
        #
        #Args:
        #  x, a matrix, that is to be inversed
        #
        m <- NULL
        set <- function(y) {
                x  <<- y
                m  <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix, 
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
## will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x' 
        #
        #Args:
        #  x, a makeCacheMatrix object, that is to be calculated or retrieved
        #
        #Returns:
        # The inversed makeCacheMatrix object if is calculated. If it is cached, it also returns the 
        # message, 'getting cached data'.
        #
        m <- x$getinverse()
        if(!is.null(m)) {
                message('getting cached data')
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
