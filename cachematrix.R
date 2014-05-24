## Caching the Inverse of a Matrix

## makeCacheMatrix function creates a special object that 
## stores a matrix and cache's its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    minverse <- NULL         ## initialize matrix
    set <- function(y) {     ## set value of matrix
        x <<- y              ## assigns the matrix input y to global x
        minverse <<- NULL    ## empties the minverse matrix (just in case)
    }
    get <- function() x      ## get value of matrix
    setinv <- function(solve) minverse <<- solve  ## set value of inverse matrix
    getinv <- function() minverse                 ## get value of inverse matrix
    list(set = set, get = get, setinv = setinv, getinv = getinv) ## create a list of functions
}


## cacheSolve function calculates the inverse of the matrix 
## created with the makeCacheMatrix function IF inverse has not been 
## previously calculated. In case it was, it gets the inverse
## from the cache and does not redo the computation.


cacheSolve <- function(x, ...) { ## Return a matrix that is the inverse of 'x'
    
    minverse <- x$getinv()       ## is there a matrix inverse of x?
    if(!is.null(minverse)) {     ## if it is there displays a message...
        message("Getting cached data...")
        return(minverse)         ## ...and return the inverse matrix
    }
    data <- x$get()              ## if there is no inverse matrix...
    minverse <- solve(data, ...) ## ...calculate the inverse...
    x$setinv(minverse)           ## ...and save it to cache
    minverse                     ## return the inverse matrix
}