## cachematrix.R by David Ramsey

## The functions makeCacheMatrix and cacheSolve implement matrices with cached
## inverses. Create a cacheMatrix with makeCacheMatrix(x), and access its
## inverse with cacheSolve(x).
## makeCacheMatrix is adapted directly from Roger Peng's makeVector, since
## it's very straightforward. cacheSolve is made simpler to better indicate
## what it's doing.

## makeCacheMatrix(x) takes a matrix and returns a list of functions:
## get() returns the matrix.
## set(y) changes the matrix and resets the cached inverse to NULL, indicating
## that it needs to be recomputed. This functionality is necessary in order
## for these functions to determine whether the matrix has been changed.
## getinverse() returns the inverse of the matrix (using caching).
## setinverse(inverse) caches the inverse of the matrix to "inverse".

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
            x <<- y
            i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve(x, ...) returns the inverse of the cacheMatrix x. Additional
##  arguments to solve (if solve needs to be recomputed) are passed through in
## "...".

cacheSolve <- function(x, ...) {
    ## If the inverse is NULL, then either we have never computed an inverse
    ## or we have just changed the matrix and need to recompute the inverse.
    ## Only under these circumstances do we actually run solve().
    if(is.null(x$getinverse())) {
        x$setinverse(solve(x$get()), ...)
    }
    ## This line always returns a cached inverse.
    x$getinverse()
}
