## Caching the Inverse of a Matrix:
## This function creates a special "matrix" that can cache its inverse.
makeCacheMatrix <- function(x=matrix()) {
        ## The MatrInve will store the inversion of the matrix
        MatrInve <- NULL  
        ## Set the matrix in the woring environment
        set <- function(y) {
                x <<- y
                ## Inicialize MatrInve to NULL
                MatrInve <<- NULL 
        }
        # Get the value of the matrix
        get <- function() x
        ## Set inverse of the matrix and store in MatrInve
        setInverse<- function(inverse) MatrInve <<- inverse
        ## Get the inversed matrix from MatrInve
        getInverse<- function() MatrInve
        ## Return the functions to the working enviornment
        list( set=set, get=get, 
              setInverse=setInverse, 
              getInverse=getInverse)
}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        MatrInve <- x$getInverse()
        ## Check if the matrx has been inversed.
        ## If so, it gets the inverted matrix from the MatrInve.
        if (!is.null(MatrInve)) {     
                message ("getting cached data")
                return (MatrInve)
        }
        ## Otherwise, inverse the matrix and set the inverse in the MatrInve 
        ## First, creat a matrix
        matr <- x$get()
        ## Return the inverse of matrix
        MatrInve <- solve(matr, ...)
        ## Set inverted matrix in MatrInve
        x$setInverse(MatrInve)
        ## Display result in cosole
        MatrInve
}
