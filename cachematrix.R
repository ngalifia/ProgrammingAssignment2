## These functions cache the inverse of a matrix

## This function creates a matrix object that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set=set, get=get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function computes, cashes and returns the inverse of the matrix 
## returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)
        s
}
