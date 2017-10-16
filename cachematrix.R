## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This makeCacheMatrix function creates a special "matrix", which is really a list containing a funciton to 
## (i) set the value of the matrix, (ii) get the value of the matrix, (iii) set the value of the inverse, and (iv) get the value of the inverse.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve (mat, ...)
    x$setInverse(inv)
    inv
        ## Return a matrix that is the inverse of 'x'
}
