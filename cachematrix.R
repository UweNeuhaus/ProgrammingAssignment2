## Two functions that are used to create and evaluate a "special matrix" that
## stores its numeric matrix and caches its inverse matrix.

## This function creates a "special matrix" that caches its inverse. The 
## special matrix basically consists of four functions.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    # Set the matrix to new value, inverse will become invalid
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # Get the matrix
    get <- function() {
        x
    }
    
    # Set the inverse matrix
    setInverse <- function(inverse) {
        inv <<- inverse
    }
    
    # Return inverse of the matrix
    getInverse <- function() {
        inv
    }
    
    # Create list of the preceeding functions
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function calculates the inverse of a "special matrix" x. If the inverse
## has already been calculated and stored in the cache, it return simply the
## cached value.
cacheSolve <- function(x, ...) {
    
    # Retrieve the inverse from x
    inv <- x$getInvers()
    
    # If the inverse is not NULL, its value has already been calculated and
    # cached. So simply return the precalculated value.
    if (!is.null(inv)) {
        message("getting cached data")
        return (inv)
    }
    
    # The inverse has not been calculated yet. Therefore, calculate the inverse,
    # cache the value and return it.
    message("calculate inverse freshly")
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
