## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        value.inverse <- NULL
      
        set <- function(y) {
                x <<- y
                value.inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) value.inverse <<- inverse
        getinverse <- function() value.inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

# This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        
        # If the inverse has already been calculated (and the matrix has not changed), 
        # then the cachesolve should retrieve the inverse from the cache.
        
        if(!is.null(inverse)) {
                message("Using cached data")
        } else {
                data <- x$get()
                inverse <- solve(data, ...)
                x$setinverse(inverse)
        }
        inverse
}
