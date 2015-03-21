# The makeCachMatrix and cacheSolve functions work together to calculate the
#inverse of a square mattix and store it in a cache, so that it may be retrieved
#rather than re-calculated if it has already been calculated
# NB: These functions only work for SQUARE matrices


# The makeCacheMatrix function creates a list containing functions that sets the
#value of a matrix, gets the value of that matrix, sets the inverse of that
#matrix, and gets the inverse of that matrix 
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


# The cacheSolve function checks to see if the inverse of the matrix defined in
#the makeCacheMatrix function has already been calculated. If so, it retrieves it
#from the cache. If not, it calculates it and stores it in the cache.
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
