## Programming Assignment 2 - Coursera R Programming Class
# Matrix inversion is usually a costly computation and their
# is some benefit to caching the inverse of a matrix rather
# than compute it repeatedly.

## The makeCacheMatrix function creates a special matrix
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        
        setinv <- function(solve) m <<- solve
        
        getinv <- function() m
        
        #list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The cacheSolve function computes the invers of the special
## matrix returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinv()
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        
        m <- solve(data)
        
        x$setinv(m)
        
        m
}
