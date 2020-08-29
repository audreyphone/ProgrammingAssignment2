## Put comments here that give an overall description of what your
## functions do
## The following functions will cache the inverse of a matrix

## Write a short comment describing this function
## This function will create a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        I <- NULL
        ##set matrix x
        set <- function(y) {
          x <<- y
          I <<- NULL
        }
        ##get matrix x
        get <- function() x
        ##set inverse of matrix x
        setInv <- function(i) I <<- i
        ##get inverse of matrix x
        getInv <- function() I 
        ##list of methods
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function
## This function will return the inverse of 'x'

cacheSolve <- function(x, ...) {
        ##inversion matrix
        I <- x$getInv()
        if (!is.null(I)) {
          message('Calculating the inversion...')
          return(I)
        }
        mat <- x$get()
        I <- solve(mat, ...)
        x$setInv(I)
        I
}
