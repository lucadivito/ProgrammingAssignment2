## The functions below cache the inverse of an input matrix, since it is 
## computationally expensive to compute it repeatedly

## CONSOLE example:
## source("cachematrix.R") # read the functions from the script
## x <- matrix(1:4, nrow = 2, ncol = 2) # matrix
## m <- makeCacheMatrix(x) # list
## cacheSolve(m) # no cached inverse available
## cacheSolve(m) # cached inverse available (message displayed)

## --------------------------------------------------------------------------

## makeCacheMatrix: creates a special "matrix" object (actually a list)
## that can cache its inverse. A getter and a setter method are defined 
## for both the input matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv_x <- NULL
    set <- function(y) {
        x <<- y
        inv_x <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inv_x <<- inv
    getinverse <- function() inv_x
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## --------------------------------------------------------------------------

## cacheSolve checks if the inverse of the special "matrix" returned by 
## makeCacheMatrix has already been cached. If yes, the inverse is retrieved,
## if not, it is computed, cached and returned.

cacheSolve <- function(x, ...) {
    inv_x <- x$getinverse()
    if(!is.null(inv_x)) {
        message("getting cached inverse matrix")
        return(inv_x)
    }
    matrix <- x$get()
    inv_x <- solve(matrix, ...)
    x$setinverse(inv_x)
    inv_x
}
