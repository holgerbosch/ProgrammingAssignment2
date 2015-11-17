## makeCacheMatrix creates and manages an object representing a matrix, including its inverse
## cacheSolve is returning the inversed matrix if already existing or calculates it and stores it 
## in the above matrix object

## this function manages an object representing a matrix. A matrix can be set or retrieved, and 
## it's inverse can also be set or retrieved. if the matrix is set (again), its inverse is reset to NULL

makeCacheMatrix <- function(x = matrix()) {
        inversedmatrix <- NULL
        setmatrix <- function(y) {
                x <<- y
                inversedmatrix <<- NULL
        }
        getmatrix <- function() x
        setinverse <- function(inversed) inversedmatrix <<- inversed
        getinverse <- function() inversedmatrix
        list(setmatrix = setmatrix,
			 getmatrix = getmatrix,
            setinverse = setinverse,
            getinverse = getinverse)
}


## if the inverse of the matrix is already calculated, then it is directly retrieved and returned.
## otherwise it is first calculated using the solve function, then stored and returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inversedmatrix <- x$getinverse()
        if(!is.null(inversedmatrix)) {
                message("getting cached data")
                return(inversedmatrix)
        }
        matrixdata <- x$getmatrix()
        inversedmatrix <- solve(matrixdata, ...)
        x$setinverse(inversedmatrix)
        inversedmatrix
}
