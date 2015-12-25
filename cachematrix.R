## Inverting multiple matrices is a time-consuming calculation in a loop.
## Using a cache can save time by checking whether the inverse has already been calculated.
## If the inverse has already been calculated, the solution is retrieved from the cache.



## This function creates an object that stores a matrix and caches its inverse.
## The object is in an environment different from the current environment.

makeCacheMatrix <- function(x=matrix()) {
    my_inverted_matrix <- NULL
    set <- function(y) {
        x <<- y
        my_inverted_matrix <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) my_inverted_matrix <<- solve
    getmatrix <- function() my_inverted_matrix
    list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}


## This function solves for the inverse of the matrix,
## but first checking whether the result has already been solved for.
## If so, it gets the solution from the cache and skips the calculation.

cacheSolve <- function(x=matrix(),...) {
    my_inverted_matrix <- x$getmatrix()
    if(!is.null(my_inverted_matrix)) {
        message("getting cached inverted data")
        return(my_inverted_matrix)
    }
    matrix <- x$get()
    my_inverted_matrix <- solve(matrix, ...)
    x$setmatrix(my_inverted_matrix)
    my_inverted_matrix
}


# ## Tests the functions on a 2x2 matrix.
# test_matrix <- makeCacheMatrix()
# test_matrix$set(matrix(1:4,2,2))
# cacheSolve(test_matrix)
