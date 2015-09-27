## These functions allow to cache the inverse of a matrix: makeCacheMatrix should be called first
## with an invertible matrix, cacheSolve should be called then in order to cache the inverse.

## Same idea as makeVector: create functions for setting and getting the matrix and also
## for setting and getting the inverse. Inverse matrix is stored in i

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


## This function is similar to cachemean: look for the inverse matrix in x, 
## calculate it if it is not cached and then return the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(is.null(i)) {
	        data <- x$get()
	        i <- solve(data)
	        x$setinverse(i)
        }
        i
}


