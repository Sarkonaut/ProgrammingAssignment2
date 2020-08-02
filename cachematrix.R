## makeCacheMatrix will create a special matrix with its inverse and cacheSolve 
## will compute its inverse or retrieve it if it's been solved.
## These functions are very similar to the example of mean given in this assignment

## makeCacheMatrix creates a special matrix 
## that can set the value and get the value of a matrix and set the inverse and 
## get the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
        mat_inv <- NULL
        set <- function(y){
                x <<- y
                mat_inv <<- NULL
        }
        get <- function()x
        setInverse <- function(inverse) mat_inv <<- inverse
        getInverse <- function()mat_inv
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
        

}


## cacheSolve calculates the inverse of a special matrix (set above). If the
## inverse has already been calculated, then cacheSolve will retrieve the
## inverse from the cache and will skip the computation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mat_inv <- x$getInverse()
        if(!is.null(mat_inv)){
                message("getting cached data")
                return(mat_inv)
        }
        mat <- x$get()
        mat_inv <- solve(mat, ...)
        x$setInverse(mat_inv)
        mat_inv
}
