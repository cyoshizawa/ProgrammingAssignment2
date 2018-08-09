## Programming Assignment 2 for R Programming on Coursera
## Assignment: Caching the Inverse of a Matrix
## Write a pair of functions that calculate and cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    dimx <- dim(x)
    ## initializing a square matrix of same dimensions with all NA
    inv <- matrix(nrow = dimx[1], ncol = dimx[2])
    set <- function(y) {
        x <<- y
        inv <<- matrix(nrow = dimx[1], ncol = dimx[2])
    }
    get <- function() x
    setinv <- function(invnew) inv <<- invnew
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated and 
## cached, then cacheSolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
    invx <- x$getinv()
    if (sum(is.na(invx)) == 0) {
        message("inverse previously calculated, getting cached inverse")
        return(invx)
    }
    matrix1 <- x$get()
    invx <- solve(matrix1)
    x$setinv(invx)
    ## Return a matrix that is the inverse of 'x'
    invx    
}
