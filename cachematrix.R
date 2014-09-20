## The makeCacheMatrix function basically encapsulates a matrix and its inverse. 
## When the function is invoked a list containing five functions is returned:
##
## - set: set a matrix
## - get: return the matrix
## - setInverse: set the inverse of a matrix
## - getInverse: return the inverse of a matrix
## - isCacheEmpty: returns TRUE if inverse is missing, otherwhise FALSE
##
## When invoking makeCacheMatrix a matrix can be provided as parameter. If no
## parameter is given, an empty matrix will be created.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        ## the '<<-' operator ensures that y is assigned to the x variable
        ## defined in the environment of the makeCacheMatrix-function
        x <<- y
    }
    get <- function() {
        x
    }
    setInverse <- function(i) {
        ## the '<<-' operator ensures that i is assigned to the inverse variable
        ## defined in the environment of the makeCacheMatrix-function
        inverse <<- i
    }
    getInverse <- function() {
        inverse
    }
    
    ## just a simple utility function which helps to keep the 
    ## cacheSolve-implementation below a bit shorter
    isCacheEmpty <- function() {
        is.null(inverse)
    }
    
    ## returns a list that enables us to invoke the defined functions
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse, 
         isCacheEmpty = isCacheEmpty)
}


## The cacheSolve-function returns the inverse of a matrix provided by the
## makeCacheMatrix-function from above.
## 
## First, it is checked whether the inverse-cache of the given x "matrix" is 
## empty. If so, the inverse of the matrix hold by x is calculated and stored
## in x. If it is not empty, we can asume that x holds a current version of the
## matrix's inverse, so there is no need to calculate it. Eventually, the 
## inverse is returned.

cacheSolve <- function(x, ...) {
    if (x$isCacheEmpty()) {
        inverse <- solve(x$get())
        x$setInverse(inverse)
    }
    
    x$getInverse()
}
