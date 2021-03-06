## makeCahceMatrix takes a matrix as an argument (assumes it follows the rules such as being square matrix and is invertible) and returns an object with get and set methods for that matrix
## an object with the set and get method for the matrix. The Set method simply calls the R function to calculate the inverse of the matrix
## called 'solve' 
## After makeCahceMatrix is called, the cacheSolve function can be called to cache the inverse of the matrix when called
## for the first time and to retrieve the cached value on subsequent calls

## makeCacheMatrix returns set and get methods for a matrix. Set calls the R method 'solve' to calculate the inverse of the matrix
##                   get method is used to retrieve the inverse of the matrix if already exists in the cache

##Examples of how to use makeCacheMatrix and cacheSolve
##Example 1 - 2 by 2 matrix
##matrix1<-matrix(c(1,2,3,4),2,2)  # creates 2 by 2 matrix
##matrix1Object<-makeCacheMatrix(matrix1)
##cacheSolve(matrix1Object)
##Example 2 - 3 by 3 matrix
##matrix2<-matrix(c(1,2,3,4,5,6,7,8,10),3,3)  # creates 3 by 3 matrix
##matrix2Object<-makeCacheMatrix(matrix2)
##cacheSolve(matrix2Object)



makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverseMatrix <- function(solve) m <<- solve
        getInverseMatrix <- function() m
        list(set = set, get = get,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)



}


## cacheSolve takes the object returned by makeCacheMatrix as the argument. Purpose is to return 
## the inverse of the matrix from the cache if already exists, and call 'solve' by accessing the makeCacheMatrix object set 
## method and store the inverse of the matrix in cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getInverseMatrix()
        if(!is.null(m)) {
                message("Getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverseMatrix(m)
        m

        
        
}
