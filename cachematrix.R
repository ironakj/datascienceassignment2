## Put comments here that give an overall description of what your
## functions do

## First Function makeCacheMatrix to create an object with matrix and associated set-get functions

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

## Second Function cacheSolve to cache inverse computation, i.e. if inverse is not present calculate using solve(X) else if present return that
## If condition here checks for presense of inverse and skips furthur computation 

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
}


