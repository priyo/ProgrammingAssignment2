## The below functions demonstrate how to cache potentially time consuming
## operations within a function.  The implementation takes advantage of
## scoping rules of the R language.  It uses the special assign operator <<- 
## explained here http://cran.r-project.org/doc/manuals/R-intro.html#Scope
## Example usage:
##   cachemat <- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))
##   cachemat$get()        # return the matrix
##   cachemat$getinverse() # returns null as inverse hasn't been calculated
##   cacheSolve(cachemat)  # calculates and returns inverse
##   cacheSolve(cachemat)  # returns cached inverse

## makeCacheMatrix function creates a special "matrix" object that can 
## cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then cacheSolve retrieves the inverse from
## the cache
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached matrix inverse")
        return(inv)
    } else {
        message("Calculating matric inverse")
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinverse(inv)
    inv
}