## These set of functions creates an object that stores a matrix and
## caches it's inverse

## makeCacheMatrix creates a vector to get/set the matrix and it's inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) i <<- inv
    getInverse <- function() i
    list(set = set, get = get, setInverse = setInverse, 
         getInverse = getInverse)
}


## cacheSolve checks to see if the inverse has been calculated. If so,
## retrieves the cache, otherwise, calculates and stores the inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if (!is.null(i)) {
        message("getting cache")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
