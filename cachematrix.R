## The cachematrix.R package contains two functions:
## makeCacheMatrix - stores a matrix and a cached inverse
## cacheSolve - returns cached inverse or computes it

## makeCacheMatrix is a container object which houses the matrix,
## along with a set of helper functions to get, set the data as 
## well as it's inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve - takes in makeCacheMatrix object
## returns cached data and message if cached value exists
## otherwise computes inverse of matrix, stores and returns it

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
