## Put comments here that give an overall description of what your
## functions do

## These functions display the basis to build a "dictionary" to provide 
## fast lookups based on "keys" (the value used to search) to obtain
## the output values (here, a computed inverse matrix) which is only
## computed once. Dictionary are commonly used in other object oriented
## languages to look up data in a cache rather than recompute them.

## Write a short comment describing this function

## makeCacheMatrix creates an object to store:
## (1) the key, here matrix x,
## (2) the computed inverse matrix s
## (3) access functions to get and set matrix x and inverse matrix s

## Note that value x and s will be manipulated outside of the current
## environment with the help of the <<- operator.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Write a short comment describing this function

## cacheSolve is a function that uses the object created by makeCacheMatrix
## and determines if the computed value s (the inverse of matrix x) is cached,
## or needs to be computed. The output of function cacheSolve is the the
## inverse of matrix x.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s        
}

## Sample test code
## x <- matrix(c(2, 0, 4, 1, 3, 1, 1, -1, 2), nrow=3, ncol=3) ##create matrix x
## z <- makeCacheMatrix(x) ## store matrix x in the cache
## z$get() ## read matrix x, just to see that it is cached.
## cacheSolve(z) ## first time, inverse matrix is computed and stored in cache.
## cacheSolve(z) ## second time, the inverse matrix is read from the cache.
