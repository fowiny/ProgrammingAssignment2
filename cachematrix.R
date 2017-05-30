## A pair of functions able to cache the inverse of a matrix, a 
## potentially time-consuming/costly computation.


## Function creating a special "matrix" object that can cache its inverse.
## The operations are; get/set the value of matrix and inverse.

makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
		set <- function(y) {
			x <<- y
			m <<- NULL
		}
		get <- function() x
		setInverse <- function(inverse) m <<- inverse
		getInverse <- function() m
		list(set = set, get = get,
	    	setInverse = setInverse,
	    	getInverse = getInverse)
}


## Function calculates the inverse of special "matrix" returned 
## by makeCacheMatrix. Performs check and gets/calculates (and sets inverse).

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        mat <- x$get()
        m <-solve(mat, ...)
        x$setInverse(m)
        m
}
