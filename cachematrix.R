## Put comments here that give an overall description of what your
## functions do

## INITIALISING OBJECTS- (x = a matrix, iv(inverse) is set to be an empty 
##                        object)
## DEFINING OBJECTS- 1) set function (y) writes x into the parent environment 
##                      and sets iv to null
##                  2)get <- returns the matrix x
##                  3) setmatrixiv sets the iv in the parent environment using 
##                      dummy argument parentmatrixiv
##                  4)getmatrixiv returns the matrix inverse
## LIST- creates a list of the functions in makeCacheMatrix enabling them to be 
## used by cacheSolve

makeCacheMatrix <- function(x = matrix()) {
    iv <- NULL
    set <- function(y) {
             x <<- y
            iv <<- NULL
    }
    get <- function()x
    setmatrixiv <- function(parentmatrixiv) iv <<-parentmatrixiv
    getmatrixiv <- function() iv
    list(set = set, get = get, 
         setmatrixiv = setmatrixiv, 
         getmatrixiv = getmatrixiv)
}


## Cachesolve function 
##  1- uses getmatrix function to return the inverse from the parent environment
##  2- uses an if statement to identify if the returned object is an inverse or 
##    NULL,
##        if inverse it will display a message and eventually return an inverse 
##        matrix
##        if NULL the function accesses the matrix x, creates its inverse matrix 
##        using solve(x), sends the matrix inverse to the cache as iv and prints
##        the inverse matrix to the console.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          iv <- x$getmatrixiv()
          if(!is.null(iv)){
                  message("getting cached data")
                  return(iv)
          }
          data <-x$get()
          iv <-solve(data, ...)
          x$setmatrixiv(iv)
          iv
}
