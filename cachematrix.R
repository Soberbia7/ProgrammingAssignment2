## Put comments here that give an overall description of what your
## functions do

## Creates a special matrix to cache an inverse

makeCacheMatrix <- function(x = matrix()) {
        
                h <- NULL
        res <- function(y) {
                x <<- y
                h <<- NULL
        }
        ormat <- function() x
        invmat <- function(solve) h <<- solve
        check <- function() h
        list(res = res, ormat = ormat, invmat = invmat, check = check)
}


## Returns the inverse of the matrix, if the inverse has been alredy calculated(and the matrix is the same)
## then the cachesolve returns the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        z <- x$check()
        if(!is.null(z)) {
                message("getting cached data")
                return(z)
        }
        other <- x$ormat()
        z <- solve(other, ...)
        x$invmat(z)
        z
}