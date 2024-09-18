## I am attempting to write functions that will serve to make a cached matrix 
## as well as solve for the inverse

## This first function will make a cached matrix

makeCacheMatrix <- function(x = matrix(sample(1:50,9),3,3)) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function()inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## this function will help solve for the inverse of the matrix if it has not 
## already been calculated

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting the cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}