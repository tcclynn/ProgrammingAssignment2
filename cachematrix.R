## Put comments here that give an overall description of what your
## functions do

## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly. It is a project to understand lexical scoping rules.

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.
## input  : matrix
## outputs :
## set() : set the value variables
## get() : get the value of the matrix
## getinverse(): get the value of the inverse matrix
## setinverse(): set the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        ss <- NULL
        set <- function(y) {
               x <<- y
               ss <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) ss <<- solve
        getinverse <- function() ss
        list( set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)
                
}


## Write a short comment describing this function

## This function computes the inverse of the special "matrix" retured by makeCacheMatrix,
## If the inverse hasalready been calculated (and the matrix has not changed), 
## the the cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        cm <- x$getinverse()
        
        if(!is.null(cm)) {
                message("getting cached data")
                return(cm)
        } 
        
        data <- x$get()
        cm <- solve(data, ...)
        x$setinverse(cm)
        cm
}

# cacheSolve(mat)
# SIMPLE TEST
# m <- matrix(c(-1,-2,1,1),2,2)
# x <- makeCacheMatrix(m)
# x$get()
# inv <- cacheSolve(x)
# inv
# inv <- cacheSolve(x)
# inv

