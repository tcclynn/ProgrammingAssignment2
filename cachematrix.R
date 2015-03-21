## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The function creates a special "matrix" object that can cache it's inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
               x <<- y
               inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list( set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)
                
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        res <- makeCacheMatrix(x)
        ck <- res$getinverse()
        
        if(!is.null(ck)) {
                message("getting cached data")
                return(ck)
        } else {
                data <- res$set()
                i <- solve(data, ...)
                res$setinverse(i)
                return(i)
        }
}

# cacheSolve(c(1,2,3,4))
# cacheSolve(mat)

