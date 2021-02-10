## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The first function makeCacheMatrix saves a squared matrix in a temporary location.
## It then invertes the matrix with a solve function and returns a list of the inverted matrix.

makeCacheMatrix <- function(x = matrix()) {

  s <- NULL
  set <- function(y) {
    
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) s <<- solve
  getinverse <- function() s
  
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}

## Write a short comment describing this function
# The chacheSolve function extracts the inverted matrix of makeCachMatrix and returns the inverted matrix.
# If the matrix is empty, the function calculates the inverted matrix itself and returns the result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    s <- x$getinverse()
    if(!is.null(s)){
      message("getting cached data")
      return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setinverse(s)
    s
}
