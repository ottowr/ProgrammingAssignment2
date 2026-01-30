## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a matrix object that stores its value 
## and then caches the inverse of the matrix object so that it is not re-calculated

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(inverse) inv <<- inverse
    
    getInverse <- function() inv
    
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
}


## Write a short comment describing this function

## This functions calculates the inverse of the special matrix created by 
## makeCacheMatrix. If the inverse has already been calculated, 
## it retrieves the cached inverse instead of recalculating it.


cacheSolve <- function(x, ...) {
    
    inv <- x$getInverse()
    
    if (!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
    
}
