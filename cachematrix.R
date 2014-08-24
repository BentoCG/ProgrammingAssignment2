## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creates a new object which as a copy of a matrix 'x' that can 
# store the value of its' inverse matrix in the cache.


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


## Write a short comment describing this function
# This function calculates and stores in the cache the invert matrix of the
# 'CacheMatrix' object 'x'; Once the invert matrix obtained with the 'solve'
# method is stored in the cache, further trials to calculate 'x' using this 
# function will simply get the value stored in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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

