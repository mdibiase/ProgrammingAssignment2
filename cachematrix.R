## Given that matrix inversion is a costly operation, these two 
## functions cache the inverse of a given matrix.

## This function creates a special "matrix" object (i.e. it returns
## a list of functions) that can cache the inverse of the given 
## matrix. 

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
  }
    get <- function() x
    setInverse <- function(solve) i <<- solve
    getInverse <- function() i
    list(set=set, get=get,
    setInverse=setInverse,
    getInverse=getInverse)
    }


## This function computes the inverse of the matrix returned by the 
## function above. If the inverse has already been calculated, it 
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
	i <- x$getInverse()
    if(!is.null(i)) {
        message("getting data cache")
        return(i)
  }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
        ## Return a matrix that is the inverse of 'x'
}
