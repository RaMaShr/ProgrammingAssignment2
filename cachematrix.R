## Defines a class that stores a matrix and allows for the inverse to of the matrix to be cached.

## Creates class to store matrix and cached inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(MatrixInverse) inverse <<- MatrixInverse
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}
    

## Checks if cached inverse for matrix exists. Outputs cached inverse if so, else calculates, caches and outputs 
## matrix inverse

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
        ## Return a matrix that is the inverse of 'x'
}
