## Put comments here that give an overall description of what your
## functions do

## Function makeCasheMatrix creates a wrapper around matrix passed
## to this function as x, additional functionality added is caching
## of matrix inverse, use function set to initialize or change the matrix

makeCacheMatrix <- function(x = matrix()) {
    cachedInverse <- NULL

    set <- function(y) {
        x <<- y
        cachedInverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) cachedInverse <<- inverse
    getinverse <- function() cachedInverse

    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse
    )
}


## Function which complements matrix caching wrapper, this function
## calculates inverse matrix only if it was not calculated before

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("Getting cached data")
        return(inv)
    }
    inv <- solve(x$get(), ...)
    x$setinverse(inv)
    inv
}
