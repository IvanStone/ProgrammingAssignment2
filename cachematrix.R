## makeCacheMatrix: This function creates a special "matrix" object that can 
##  cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" 
##  returned by makeCacheMatrix above. If the inverse has already been 
##  calculated (and the matrix has not changed), then the cachesolve 
##  should retrieve the inverse from the cache.


## makeCacheMatrix creates a special matrix with additional functions baked
##  in to enable caching of the matrix and inverse version of the matrix

makeCacheMatrix <- function(x = matrix()) {
    ix <- NULL
    set <- function(y) {
        x <<- y
        ix <<- NULL
    }
    get <- function() { x }
    setInverse <- function(inverse) { ix <<- inverse }
    getInverse <- function() { ix }
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## cacheSolve uses the special matrix to create an inverse version of the 
##  matrix and cache it. If run mulitple times with the same special matrix
##  it will return the cached version instead of processing the matrix again

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    data <- x$get()
    
    ix <- x$getInverse()
    if(!is.null(ix)) {
        message("get cached data")
        return (ix)
    }
    else {
        message("get inverse and cache")
        ix <- solve(data)
        x$setInverse(ix)
    }
    ix  
}

