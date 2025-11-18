## makeCacheMatrix: creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL  # variable to store matrix inverse

    # Set the matrix and clear previous inverse
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    # Get the matrix
    get <- function() x

    # Set the inverse
    setinverse <- function(inverse) inv <<- inverse

    # Get the inverse
    getinverse <- function() inv

    # Return a list of the above functions
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve: computes the inverse of the matrix returned by makeCacheMatrix
## If inverse is already cached, retrieves it instead of recomputing
cacheSolve <- function(x, ...) {

    inv <- x$getinverse()

    # If inverse already stored, return it
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }

    # Otherwise compute inverse
    mat <- x$get()
    inv <- solve(mat, ...)  # compute inverse

    # Cache the inverse
    x$setinverse(inv)

    # Return inverse
    inv
}
