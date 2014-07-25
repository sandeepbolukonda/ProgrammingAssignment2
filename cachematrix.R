## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  # initialize 
  m <- NULL
  
  # method for setting the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # method for getting the matrix
  get <- function() {x}
  
  # method for setting the inverse of the matrix  
  setinv <- function(cacheSolve) m <<- cacheSolve
  
  # method for getting the inverse of the matrix  
  getinv <- function() m
  
  # return the above methods as a list
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  # assign the return value, which is the inverse of the matrix
  m <- x$getinv()
  
  # if the result is already set, return the inverse of the matrix
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #  otherwise, determine the inverse using the solve function
  data <- x$get()
  m <- solve(data) %*% data
  
  # set the inverse of the matrix
  x$setinv(m)
  
  # return the inverse
  m
}
