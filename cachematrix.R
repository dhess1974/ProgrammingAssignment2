## Programming Assignment 2
## David Hess

## This code reads in matrix x and sets a variable my_inverse
## Creates the cache. This will return my_inverse as NULL

makeCacheMatrix <- function(x = matrix()) {
        my_inverse <- NULL
        set_matrix <- function(y) {
                x <<- y
                my_inverse <<- NULL
        }
        ## get_matrix simply calls the matrix x
        get_matrix <- function() x
        setinverse <- function(solve) my_inverse <<- solve
        getinverse <- function() my_inverse
        list(set_matrix = set_matrix, get_matrix = get_matrix, setinverse = setinverse, getinverse = getinverse)
}


## This code returns the mean from cache if it has been calculated or calculates it and stores it in cache
## The first time this is called after makeCacheMatrix is called, my_inverse will be NULL and it will
## calculate the inverse. The next time it is run, my_inverse will have a value in it and the function
## will return that value from cache

cacheSolve <- function(x, ...) {
        my_toggle <- x$getinverse()
        ## the following checks if my_inverse is not NULL, if so it gets the inverse from cache
        ## the return command exits the function at that point
        if(!is.null(my_inverse)) {
                message("getting cached data")
                return(my_inverse)
        }
        ## when the above condition is not met, then the function continues and solves the inverse
        data <- x$get_matrix()
        my_inverse <- solve(data, ...)
        x$setinverse(my_inverse)
        my_inverse
        ## Return a matrix that is the inverse of 'x'
}
