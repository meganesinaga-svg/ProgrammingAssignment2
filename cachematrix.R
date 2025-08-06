## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL # to store the inverse
  # Set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # Get the matrix
  get <- function() x
  # Set the inverse
  setinverse <- function(inverse) inv <<- inverse
  # Get the inverse
  getinverse <- function() inv
  # Return a list of all the functions above
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # Calculate the inverse
  data <- x$get()
  inv <- solve(data,...)
  x$setinverse(inv)  
  # cache the inverse
  inv
}