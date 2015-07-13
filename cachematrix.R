## This script was done as part of R programming Coursera course provided by John Hopkins
## It creates two functions that can be used in conjuction to cache 
## the inverse of a matrix which can be a compute heavy task

## The first function creates the object that can be passed into the second function for caching.
## it returns a list with a setter and getter for the matrix. 
## The list also contains a setinverse and getinverse for setting the inverse of the passed in matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the matrix returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
