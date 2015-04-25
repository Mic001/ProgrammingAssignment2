## This function creates a special "matrix" object that can cache its inverse

## The first function, makeCacheMatrix, creates a special "matrix" which in
## effect is a list containing functions to set the value of a matrix, 
## get the value of a matrix, set the inverse of the matrix, get the inverse
## of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverseM) inverse <<- inverseM
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve computes the inverse of the special Matrix which was created
## with the above function (makeCacheMatrix). However cacheSolve will check
## if the inverse of the matrix has already been computed. If so, it gets the
## the inverse from the cache and skips the computation. Otherwise, it 
## calculates the inverse of the matrix and sets the value of the inverse
## matrix in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
