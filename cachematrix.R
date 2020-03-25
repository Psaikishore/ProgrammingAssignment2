## Writing these set of functions because matrix inversion 
## is an expensive operation. It would be beneficial to
## save the inverse in case the operation is done repeatedly. 
## The below functions do this for us.

## This functions creates a custom "matrix" 
## and  adds caching functionality to it

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(matrix_inverse) inverse <<- matrix_inverse
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function calculates the inverse of the custom "matrix" created with the above function.
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value of the mean in the cache via the setinverse function.
## (It is assumed that the matrix is invertible)

cacheSolve <- function(x, ...) {
        curr_inverse <- x$getinverse()
        if(!is.null(curr_inverse)) {
                message("getting cached data")
                return(curr_inverse)
        }
        data <- x$get()
        req_inverse <- solve(data, ...)
        x$setinverse(req_inverse)
        req_inverse
}
