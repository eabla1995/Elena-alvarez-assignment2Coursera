# Elena-alvarez-assignment2Coursera
##The function makeCacheMatrix will:
##construct mutator and accessor methods from a normal matrix.
##secondly, taking arguments from the global environment to another 
##which is the intern one, the function will access to the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
  x <<- y
  inv <<- NULL
}
get <- function() x
setinv <- function(mat) inv <<- mat
getinv <- function() inv
list(set = set, get = get,
     setinv = setinv,
     getinv = getinv)
}

## Write a short comment describing this function
## cacheSolve function will calculate the inverse of a caches' matrix (obtained by
## the above function).
##We need to have the matrixcalc library to achieve our goal. 

cacheSolve <- function(x, ...) {
  library(matrixcalc)
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- matrix.inverse(data, ...)
  x$setinv(inv)
  inv
}
