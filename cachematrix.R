## cachematrix.R

##      The list of functions that are used
##      to store the matrix object and cache its inversion:

##      1. set the value of the matrix
##      2. get the value of the matrix
##      3. set the value of the inverted matrix
##      4. get the value of the inverted matrix

## makeCacheMatrix: This function creates a special "matrix" object
## of the original and inverted matrix values.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve: This function computes the inverse of the special
## "matrix" returned by makeCacheMatrix above. If the inverse has
## already been calculated (and the matrix has not changed), then
## cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
