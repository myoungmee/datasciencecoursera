## These functions below cache the inverse of a matrix.

## makeCachematrix creates a special "matrix" objectthat can cache its inverse.
makeCacheMatrix <- function(matr = matrix()) {
    inv <- NULL                      ## Initialize the inverse property
    set <- function(matrix) {
        matr <<- matrix
        inv <<- NULL
    }
    get <- function() matr 
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

cacheSolve <- function(m, ...) {
    inv <- matr$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- matr$get()
    inv <- inverse(data, ...)
    matr$setInverse(inv)
    inv
}