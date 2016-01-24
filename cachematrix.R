## 2 Assignment: Cache the inverse of a matrix

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function:
# The makeCacheMatrix function creates a special "matrix", which is a list of the
# results of the following functions: set the matrix value; get the matrix value,
# set the inverse of the matrix; get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
            x <<- y
            inv <<- NULL
        }
        get <- function() x
        setInverse  <- function(solve) inv <<- solve
        getInverse  <- function() inv
        list(set = set, get = get, setInverse  = setInverse , 
             getInverse  = getInverse)
    }


## Write a short comment describing this function
# The cacheSolve function calculates the inverse of the special "matrix" created
# with the makeCacheMatrix function. However, it first checks to see if the inverse
# has already been calculated. If so, it gets the inverse from the cache. Otherwise, 
# it calculates the inverse of the matrix and sets it in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
        inv <- x$getInverse()
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
    }
