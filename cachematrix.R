## This is an implementation of a pair of functions that cache
## the inverse of a matrix.


## This function creates a special "matrix" object that can cache 
## its inverse.
makeCacheMatrix <- function(x = matrix()) {
        
        xi <- NULL
        
        setMatrix <- function(y) {
                x <<- y
                xi <<- NULL
        }
        
        getMatrix <- function() {
                x
        }
        
        setInverse <- function(invx) {
                xi <<- invx
        }
        
        getInverse <- function() {
                xi
        }
        
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
        
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        xinv <- x$getInverse()
        
        if(!is.null(xinv)) {
                message("Getting cached Matrix")
                return(xinv)
        }
        
        xmatrix <- x$getMatrix()
        xinv <- solve(xmatrix)
        x$setInverse(xinv)
        
        xinv
}
