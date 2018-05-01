## The function makeCacheMatrix creates a special "matrix" object that
## can cache its inverse. It returns a list containing set/get functions 
## to get the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    set_inverse <- function(solve) i <<- solve
    get_inverse <- function() i
    list(set = set, get = get, 
         set_inverse = set_inverse, get_inverse = get_inverse)
}

## The function cacheSolve computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    i <- x$get_inverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$set_inverse(i)
    i   
}

