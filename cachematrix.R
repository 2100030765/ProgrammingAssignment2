## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inversa <- NULL                             ## initialize inv as NULL; will hold value of matrix inverse 
    set <- function(y) {                    ## define the set function to assign new 
      x <<- y                             ## value of matrix in parent environment
      inversa <<- NULL                        ## if there is a new matrix, reset inv to NULL
    }
    get <- function() {x}                     ## define the get fucntion - returns value of the matrix argument
    
    setInversa<- function(inversacalculada){ inversa <<- inversacalculada}  ## assigns value of inv in parent environment
    getInversa <- function() {inversa }                    ## gets the value of inv where called
    list(set = set, get = get, setInversa = setInversa, getInversa = getInversa)  ## you need this in order to refer 
    ## to the functions with the $ operator
  

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inversa <- x$getInversa()
    if(!is.null(inversa)) {
      message("getting cached data")
      return(inversa)
    }
    data <- x$get()
    inversa <- solve(data, ...)
    x$setInverse(inversa)
    inversa
}
