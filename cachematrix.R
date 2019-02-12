## These functions calculate the inverse of a given n-by-n matrix. The computed matrix
## will be stored in the cache for rapid future use.

## This function creates a list containing function to set and get yhe value of the matrix
## and set and get the value of the inverse matrix.

makeCacheMatrix<- function(x = matrix()) {
  matrixinverse <- NULL
  set <- function(y) {
    x <<- y
    matrixinverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) matrixinverse <<- solve
  getinverse <- function() matrixinverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function that computes the inverse of the matrix returned by the function makeCacheMatrix
## If the inverse was already computed -by makeCacheMatrix- then this value will be used.

cacheSolve <- function(x, ...) {
  matrixinverse <- x$getinverse()
  if(!is.null(matrixinverse)) {
    message("getting cached data")
    return(matrixinverse)
  }
  data <- x$get()
  matrixinverse <- solve(data, ...)
  x$setinverse(matrixinverse)
  matrixinverse
}