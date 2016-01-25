# Yiqun Sun
# Coursera R Programming
# Assignment 2
# 01/24/2016

## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix.
## This file contains two functions that cache the inverse of a matrix.




##  This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the  
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}


### Test
my_matrix1 = matrix(rnorm(9), nrow = 3, ncol = 3)
my_matrix2 = matrix(rnorm(16), nrow = 4, ncol = 4)

special_matrix = makeCacheMatrix(my_matrix1)
special_matrix
special_matrix$get()
special_matrix$getinverse()

cacheSolve(special_matrix)
special_matrix$getinverse()
cacheSolve(special_matrix)

special_matrix$set(my_matrix2)
special_matrix$get()
special_matrix$getinverse()

cacheSolve(special_matrix)
special_matrix$getinverse()
cacheSolve(special_matrix)
