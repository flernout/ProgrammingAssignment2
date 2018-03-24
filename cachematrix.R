# The following function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inversedMatrix <- NULL
        set <- function(y) {
                x <<- y
                inversedMatrix <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inversedMatrix <<- inverse
        getInverse <- function() inversedMatrix
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

# This function computes the inverse of the object created by 
# makeCacheMatrix. If the inverse has already been calculated (and the 
# matrix has not changed), then it should retrieve the inverse from the cache.

# Return the inverse of 'x' matrix
cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}

## Test
# source("cachematrix.R")
# matrix1 <- matrix(11:14, 2, 2)
# my_matrix <- makeCacheMatrix(matrix1)
# my_matrix$get()
# my_matrix$getInverse()
# Result: NULL
# cacheSolve(my_matrix)
# solve(matrix1)
# cacheSolve(my_matrix)
# Result: getting cached data
