## function makeCacheMatrix creates a special "matrix", which stores both
## the matrix and its computed inverse, returning a list of four access functions
## cacheSolve computes the inverse of the matrix for the first call
## and stores the result in cache, avoiding to recompute for subsequent calls


## makeCacheMatrix uses as an argument a common R matrix to construct a 
## cached version matrix, returning the list of access functions

makeCacheMatrix <- function(x = matrix()) {
        ## Provides a list of functions to access a matrix and its inverse
        inv_x <- NULL
        set <- function(y) {
                x <<- y
                inv_x <<- NULL
        }
        get <- function() x
        setinv <- function(inv) inv_x <<- inv
        getinv <- function() inv_x
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve uses as an argument a cached version matrix for optimizing 
## inverse computation, which is done only on first call

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_x <- x$getinv()
        if(!is.null(inv_x)) {
                message("getting cached matrix")
                return(inv_x)
        }
        mat <- x$get()
        inv_x <- solve(mat, ...)
        x$setinv(inv_x)
        inv_x
}
