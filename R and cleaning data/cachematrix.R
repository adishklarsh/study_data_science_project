
## Two functions that cache the inverse of a matrix. makeCacheMatrix is an object made of a matrix
## and its inverse and cacheSolve computed or retrieves the inverse of the matrix in the makeCacheMatrix object
## Matrix inversion is usually a costly computation and there may be some benefit to caching the 
## inverse of a matrix rather than compute it repeatedly

## This function creates a special "matrix" object that can cache its inverse.
## The object variables are: x - matrix, inverse = matrix
## Constructor(matrix) - initializes x = matrix, inverse = NULL
## Returns a list of functions: 
## set(matrix) - initialize x = matrix, inverse = NULL
## get() - returns x
## setInverse(invertedMatrix) - sets inverse = invertedMatrix
## getInverse() - returns inverse

makeCacheMatrix <- function(x = numeric()) {
    inverse <- NULL
    set <- function(matrix) {
        x <<- matrix
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inverted) inverse <<- inverted
    getInverse <- function() inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}



## This function returns the inverse of x - a special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## retrieves the inverse from the cache, otherwise the inverse is computed.

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    inputMatrix <- x$get()
    inverse <- solve(inputMatrix,diag(nrow = nrow(inputMatrix), ncol = ncol(inputMatrix)))
    x$setInverse(inverse)
    inverse
}

