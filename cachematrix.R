## 2 functions manipulating matrix :
## - makeCacheMatrix puts a matrix into cache
## - cacheSolve returns the inverse of the matrix and puts it into cache
## if it is not already in.

## This function creates a list object that :
## - setMatrix : sets and caches the matrix
## - getMatrix : gets from the cache the matrix
## - setInverse : sets and caches the inverse of the matrix
## - getInverse : gets from the cache the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  setMatrix <- function(y) {
    x <<- y
    i <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The following function calculates the inverse of matrix in the list 
## that MakeCacheMatrix function created.
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and 
## sets the result in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x', 
        ## we assume that x is squared and reversible.
  i <- x$getInverse()
  if(!is.null(i)) {
    ## inverse already calculated then gets it.
    message("getting cached Inverse Matrix")
    return(i)
  }
  ## no inverse matrix found, then calculates it.
  matrix <- x$getMatrix()
  i <- solve(matrix, ...)
  x$setInverse(i)
  i
  
}
