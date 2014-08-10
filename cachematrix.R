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
    # Variable inv will be used to store the inverse of x matrix
    inv <- NULL
    
    # set function to set the value of the matrix, with each set
    # the inverse value is reset to NULL
    set <- function(y) 
    {
        x <<- y
        inv <<- NULL
    }
    
    # get function to get the vaue of the matrix
    get <- function() x
    
    # setinv function to set (not calculate) the inv variable
    setinv <- function(inverse) inv <<- inverse
    
    # getinv function to get the value of the inv variable
    getinv <- function() inv
    
    # Returning a list of four functions
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
    
    # Check to see if inv is previously set or not
    if(!is.null(inv)) 
    {
        # Previously set, get the cached value and return
        message("getting cached inverse")
        return(inv)
    }
    
    # If previously not set, then get the matrix and calc inverse
    data <- x$get()
    inv <- solve(data)
    
    # Also set (cache) the inv value for future use
    x$setinv(inv)
    
    # Return the value of the matrix inverse
    inv
}
