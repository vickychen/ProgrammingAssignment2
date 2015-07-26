## Put comments here that give an overall description of what your
## functions do



## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    #solve(X) returns its inverse
    #Initialize m 
    m <- NULL
    
    #Set the value of matrix x
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    #Get the function for calculation
    get <- function() x
    
    #Set the value for inverse
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}



## Computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # Get m if it exists
        m <- x$getInverse()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        
        # Calculate if it doesn't
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
