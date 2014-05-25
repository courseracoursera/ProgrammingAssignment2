#####################################################################
## Coursera: R Programming (May-June 2014)
## ASSIGNMENT 2:  CACHING THE INVERSE OF A MATRIX
#####################################################################

#####################################################################
## Create a special "matrix" object that can cache its inverse.
## 1) set the matrix
## 2) get the matrix
## 3) set the inverse of the matrix
## 4) get the inverse of the matrix
#####################################################################
makeCacheMatrix <- function(x = matrix()) {
        xInverse <- NULL
        setMatrix <- function(y) {
                x <<- y
                xInverse <<- NULL
        }
        getMatrix <- function() x
        setMatrixInverse <- function(inverse) xInverse <<- inverse
        getMatrixInverse <- function() xInverse
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setMatrixInverse = setMatrixInverse,
             getMatrixInverse = getMatrixInverse)
}


#####################################################################
## Compute the inverse of the special "matrix" returned by 
## makeCacheMatrix above. First check if the inverse matrix exists.
## If exist, print a message and retrieve the inverse from cache.
## Otherwise, calculate the inverse matrix and set it in the cache.
#####################################################################
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        Inverse <- x$getMatrixInverse()

        ## If inverse exists and unchanged, return message & inverse matrix
        if(!is.null(Inverse)) {
                message("getMatrixting cached data")
                return(Inverse)
        }
        data <- x$getMatrix()

        ## Computing inverse of a square matrix with the 'solve' function
        Inverse <- solve(data, ...)
        x$setMatrixInverse(Inverse)
        Inverse
}
