

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a special list of functions
## that allows the caller to
##
##     set the value of the matrix
##     get the value of the matrix
##     get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    ##The cached inverse
    i <- NULL

    ## sets the matrix and resets the inverse to null
    set <- function(y) {
        x <<- y
        i <<- NULL
    }

    ## returns the matrix
    get <- function(){
        x
    }

    ## sets the inverse
    setinv <- function(inv){
        i <<- inv
    }

    ## gets the inverse
    getinv<- function() i

    ## the list of subfunctions that are return when calling the makeCacheMatrix
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
##
## The cacheSolve functions returns the inverse of the matrix x.
## It uses the makeCacheMatrix function to retrieve the inverse of
## x within that function. If the inverse is null, cacheSolve sets
## the solves the inverse and caches the result back into the makeCacheMatrix
##
cacheSolve <- function(x, ...) {
    ## get the inverse of x
    i <- x$getinv()

    ## if the inverse is already set return inverse of 'x'
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }

    ## otherwise solve fore the inverse
    data <- x$get()
    i <- solve(data, ...)

    ## and cache the inverse and returned it
    x$setinv(i)
    i
}
