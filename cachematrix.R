# This code is written as part of the Programming Assignment 2
# submission required for R Programming course. The submission
# consists of two functions, makeCacheMatrix() and cacheSolve()
# that help in caching the inverse of a matrix for reuse.

# For this assignment, we assume that the matrix supplied 
# is always invertible and use solve() function in R to calculate
# matrix inverse.


# This function creates a special "matrix" object that can 
# cache its inverse.
makeCacheMatrix <- function(x = matrix()) 
{
    inv <- NULL
    set <- function(y) 
    {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    setinv <- function(inverse) inv <<- inverse
    
    getinv <- function() inv
    
    list(set = set, get = get,setinv = setinv, getinv = getinv)

}


# This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix() above. If the inverse has already
# been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) 
{
    # Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}
