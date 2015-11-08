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
