## These functions make a way to cache the inverse of a matrix,
## thus avoiding subsequent computations of the inverse, which can
## be very time consuming

## This function creates a list of functions to set and retrieve the matrix
## passed as argument and its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function takes as argument the list returned by the function
## 'makeCacheMatrix', and retrieves the cached version of the inverse 
## of the matrix inside the list. If the inverse is not set, it is
## calculated and stored inside the list for future cached retrieval

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
