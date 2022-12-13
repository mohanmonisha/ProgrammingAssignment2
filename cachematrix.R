## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function and the cacheSolve function below leverage R's 
# Lexical scoping rules together with the  <<- assignment operator to implement
# A solution for caching the inverse of square inverse matrices.

makeCacheMatrix <- function(x = matrix()) {
  # Creates a special "matrix" object that can cache its inverse and which 
  # contains 3 functions (get, setinverse, getinverse) to access and manipulate
  # its attributes.
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  # Computes the inverse of a special "matrix" object. If the inverse has 
  # already been calculated, then the inverse is retrieved from the cache.
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
