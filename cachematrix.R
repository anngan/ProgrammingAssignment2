## R Programming Assignment 2: Lexical Scoping
## Matrix inversion is usually a costly computation and it can be beneficial to find a way of 
## caching the inverse of a matrix instead of computing it repeatedly.
## In order to make it easier, two functions can be used. 

## First function is makeCacheMatrix: this function creates a special 'matrix'
## object that can cache its inverse. This special 'matrix' object is a list that includes
## functions that set and get the value of the matrix and set and get the value of the inverse
## of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL 
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        setInv <- function(inverse) inv <<- inverse
        getInv <- function() inv
        list(set = set, get = get,
             setInv = setInv, getInv = getInv)
}

## Secondly, cacheSolve function can be used. cacheSolve function computes the inverse  
## of the special matrix that is returned by the function makeCacheMatrix. In the case that
## the inverse has been already computed and the matrix remained the same, it should
## retrieve the inverse from the created cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        m <- x$get()
        inv <- solve(m, ...)
        x$setInv(inv)
        inv
}  
