## These functions are used to calculate the inverse of an invertible matrix,
## and cache this value, so it can be accessed later without calculating it again

## a function which takes an invertible matrix, and
## returns a special matrix type, which encapsulates
## the original matrix and caches its inverse
makeCacheMatrix <- function(x = matrix()){
  
  ## Check if the given value is a matrix. 
  ## Furthermore, the matrix should be invertible (however this is unchecked at this point)
  if(!is.matrix(x)) 
    stop('The parameter should be an invertible matrix.')
  
  inverse <- NULL
  
  ## Function to set the original matrix
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  
  ## Function to get the original matrix
  get <- function() x
  
  ## Function to set the inverse of the matrix
  setInverse <- function(i)
    inverse <<- i
  
  ## Function to get the inverse of the matrix
  getInverse <- function() inverse
  
  ## Return the list containing the 4 functions above
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}

## used to get the inverse of a matrix that was
## returned by the function "makeCacheMatrix(x)" 
cacheSolve <- function(x, ...){
  
  inverse <- x$getInverse()
  
  if(!is.null(inverse))
    return(inverse)
  
  data <- x$get()
  inverse <- solve(data)
  x$setInverse(inverse)
  
  ## Return a matrix that is the inverse of 'x'
  inverse
}