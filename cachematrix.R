## This code contains 2 functions that is used for computing the inverse of a matrix,
## with cache. 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    ## set is a function that changes the matrix stored in the main function
    set <- function(y){
        x <<- y;
        inverse <<- NULL;
    }
    ## get is a function that returns the matrix x
    get <- function() return(x); 
    ## setinv is a function that stores the input into the variable "inverse" in the main function 
    setinv <- function(inv) inverse <<- inv;
    ## getinv is a function that returns the variable "inverse" from the main function
    getinv <- function() return(inverse);
    
    ## The function list is returned
    return(list(set = set, get = get, setinv = setinv, getinv = getinv))

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    ## check if there is a cache
    inverse <-x$getinv()
    if(!is.null(inverse)) {
        return(inverse)
    }
    ## get matrix data
    data <- x$get()
    inverse <- solve(data, ...)
    ## store the result in cache
    x$setinv(inverse)
    ## Return a matrix that is the inverse of 'x'
    return(inverse)
}
