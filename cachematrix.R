## Together these functions store a matrix and cache its inverse for later use.


## This function creates a matrix, stores it, and caches the inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(m) {
        x <<- m
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function checks if the inverse was already calculated, if yes the inverse is returned,
## if not it calculates and stores the inversed matrix in the cache.

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
        message("retrieving inversed matrix")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinv(i)
    i
}
