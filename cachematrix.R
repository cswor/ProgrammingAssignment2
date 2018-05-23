## Creastes vector of functions needed to implement the
## caching of the inv matrix.

makeCacheMatrix <- function(x = matrix()) {
    #myMat <- matrix(c(1,0,5,2,1,6,3,4,0), 3, 3)
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}

## Checks cache for the inv and
## returns the cached inv if found
## otherwise, solves for the inverse and caches it,
## and returns the inv

cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if ( !is.null(m) ) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinv(m)
    m
}
