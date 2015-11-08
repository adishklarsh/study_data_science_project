
## This function returns the inverse of the special "matrix" returned by makeCacheMatrix. 
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