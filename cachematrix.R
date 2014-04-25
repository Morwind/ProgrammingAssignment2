## The two functions below aim to convert any matrix(which can be solved)
## into a "special" one that can store it`s inverse and therefore save 
## time and resources and computational power.

## This funtion converts the matrix in the input into a list with four components
## some of which can set form the global environment.
## To access any of the components type "x$componentName()".

makeCacheMatrix <- function(x = matrix()) {
   s <- NULL
        
        set <- function(y) {           ## Set a new matrix defined in anothe environment.
                x <<- y
                s <<- NULL
        }
        get <- function() x            ## Shows the matrix currently used.
        
        setInverse <- function(solve) s <<- solve ## Set the inverse matrix defined in
                                                  ## another environment.
       
        getInverse <- function() s      ## Shows the inverse of 'x' (if stored).
      
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## This function will check if a matrix created with "makeCacheMatrix"
## has it`s inverse stored in it. If that is the case the function
## will show a massage"getting cached data" and return the answer.
## If not it will compute the inverse, store it in the "special"
## matrix and display the answer.

cacheSolve <- function(x, ...) {
   s <- x$getInverse()              ## Checks for stored inverse of 'x'.
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()             
        s <- solve(data, ...)       ## Computing the inverse of 'x'.
        x$setInverse(s)             ## Sets the inverse in the "special" matrix ('x').
        return(s)                   ## Return a matrix that is the inverse of 'x'
}
