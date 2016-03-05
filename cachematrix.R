## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix  creates a special "matrix" object that can cache its inverse.
## makeCacheMatrix has methods set, get, setsolve, getsolve.
makeCacheMatrix <- function(x = matrix()) { 
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## Input for this function is the special matrix created (returned) from makeCacheMatrix function
## This function display the cache value or calculate the inverse of matrix
cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, diag(1, nrow(data)))
    x$setsolve(m)
    m
        ## Return a matrix that is the inverse of 'x'
}
