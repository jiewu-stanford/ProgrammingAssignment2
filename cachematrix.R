## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below is a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    ## initialize inverse object
    m <- NULL
    
    ## define set data function
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## define get data function
    get <- function() {
        x
    }
    
    ## define set inverse function
    setinv <- function(i) {
        m <<- i
    }
    
    ## define get inverse function
    getinv <- function() {
        m
    }
    
    ## return a list of functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse from
## the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    
    ## retrieve the cached inverse if it already exists
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## if the inverse is not cached get the cached matrix to compute inverse
    data <- x$get()
    
    ## solve for the inverse
    m <- solve(data,...)
    
    ## cache the inverse
    x$setinv(m)
    
    ## return the inverse
    m
}
