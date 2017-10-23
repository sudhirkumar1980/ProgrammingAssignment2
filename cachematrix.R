## Following functions cache and compute inversion of matrix
## functions do

## makeCacheMatrix function create special matrix objects which can cache inversion of it.

makeCacheMatrix <- function(x = matrix()) {
        matrixinv <- NULL
        set <- function(y) {
                x <<- y
                matrixinv <<- NULL
        }
        get <- function() x
        setinv <- function(inv) matrixinv <<- inv
        getinv <- function() matrixinv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function cacluate inversion of special matrix object created in above function. it first check if inversion is alreadted calculated.
## If so, it will get inversion from cache and skip computation. If not, it calculate inversion and set it cache using setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matrixinv <- x$getinv()
        if(!is.null(matrixinv)){
                message("Getting Cache Data")
                return(matrixinv)
        }
        data <- x$get()
        matrix_inverse <- solve(data,...)
        x$setinv(matrix_inverse)
        return(matrix_inverse)
}
