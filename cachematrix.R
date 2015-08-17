## Script for solving and storing in cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
  # Create a matrix able to store its inverse as metadata.
  # 
  # Args:
  #   x: A square invertible matrix
  #
  # Returns:
  #   The same matrix able to store its inverse. 
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'.
  # 
  # Args:
  #   x: A 'makeCacheMatrix' object
  #   ...: Additional parameters to be sent to the solve function
  #
  # Returns:
  #   The inverse of the matrix, its inverse is stored in the 'makeCacheMatrix' object. 
  
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
