## Functions for caching the result of the computation for inverting a matrix
## First call the makeCacheMatrix with the matrix to invert to initialize the
## variables for the caching process. Then, calls to the "cacheSolve" function
## will use the scoped variables to check if the matrix already had its inverse
## computed. If it was, the result will be returned from the cache.  If not,
## the inverse of the matrix will be computed and stored in the cache.
## 
## makeCacheMatrix creates a list for the different functions for managing
## the caching. Four function are defined:
##
## set(x) : If the matrix is different than the one currently in the cache,
##          saves the new matrix "x" and clears the inverse "flag"
## get() : Returns the current save matrix
## setinv(inv) : Sets the cache invert value to "inv"
## getinv() : Gets the current invert matrix from the cache

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        if (!identical(x,y)) {  ## Is the matrix to treat different from the cache?
            x <<- y             ## If so, replace the cached one and reset the
            inv <<- NULL        ## invert flag (set to NULL)
        }
        else {
            message("Matrix already defined")   ## Matrices are the same...say it!
        }
    }
    
    ## Functions for cache process
    
    get <- function() x
    setinv <- function(invert) inv <<- invert
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Check to see if the matrix passed in the list already has it's inverse
## computed. If so, it returns it directly.  If not, the inverse is computed
## and saved in the list.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    inv<-x$getinv()         ## Check if the matrix was already inverted
    if (!is.null(inv)) {    ## A NULL value indicates it wasn't computed
        message("getting cached data")  ## If not NULL, say it and...
        return(inv)                     ##  return the inverse
    }
    
    ## The inverse was not computed for this matrix
    
    data<-x$get()           ## Fetch the matrix data from the cache
    inv<-solve(data, ...)   ## Compute its invert
    x$setinv(inv)           ## Set the invert matrix in the cache and
    inv                     ## "Print" it
}
