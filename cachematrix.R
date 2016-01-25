## Returns a list of 4 functions
# set() - set the matrix
# get() - get the matrix
# setinverse() - set the inverse of the matrix
# getinverse() - get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() { x }
    setinverse <- function(inverse) { inv <<- inverse }
    getinverse <- function() { inv }
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Returns the inverse of a matrix
# If inverse was already calculated, returns the inverse from cache
# Otherwise, calculates the inverse, stores it in cache and returns the inverse
# Assumes matrix is invertible
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("getting cached data.")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
