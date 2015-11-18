## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  
  # if the inverse is null so we will come to know it is already been calculated or not
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  
  mat <- x$get()
  ##Computing the inverse of a square matrix can be 
  ##done with the solve function in R. For example, 
  ##if X is a square invertible matrix, then solve(X) returns its inverse.
  
  i <- solve(mat, ...)
  x$setinverse(i)
  
  ## Return a matrix that is the inverse of 'x'
  return(i)
}
