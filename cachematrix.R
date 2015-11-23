## This file contains two functions that are used to calculate the inverse
## of a matrix, or if the inverse of this matrix has been calculated before 
## they retrieve the inverse from cache.

## The following function creates a list object that stores a numeric matrix
##and its cached inverse. The list contains 4 fucntions described below

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    ##function that enables changing values of a matrix
    set <- function(y) { 
        x <<- y
        i <<- NULL
    }
    
    ##function returning the stored matrix
    get <- function() x 
    
    ##function that enables storing an inversed matrix
    setinverse <- function(inverse) i <<- inverse 
    
    ##function returning the stored inversed matrix 
    getinverse <- function() i
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    ##return four above functions as a list
}


## The following function calculates the inverse of a matrix contained
## in a list created by makeCacheMatrix or retrieves it from cache

cacheInverse <- function(x, ...) {
    
    ## Checking if the inverse is already in cache
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    ## Retrieving a matrix from list and calculating its inverse
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
    ## Return inversed matrix
}
