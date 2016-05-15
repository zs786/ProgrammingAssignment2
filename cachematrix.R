## Caching the Inverse of a Matrix:
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inversal <- NULL
        set <- function(y) {
                x <<- y
                inversal <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inversal <<- inverse
        getInverse <- function() inversal
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inversal <- x$getInverse()
        if (!is.null(inversal)) {
                message("getting cached data")
                return(inversal)
        }
        mat <- x$get()
        inversal <- solve(mat, ...)
        x$setInverse(inversal)
        inversal
}
