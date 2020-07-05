## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                           
  set <- function(y) {                  
    x <<- y                          
    m <<- NULL                       
  }
  get <- function() x                   
  
  setinv <- function(inverse) m <<- inverse 
  getinv <- function() m               
  list(set = set, get = get, 
       setinverse = setinv, getinverse = getinv) 

}

## This 'makeCacheMatrix' creates a special 
## "matrix" object that can cache its inverse.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m     
}

## The 'cacheSolve' computes the inverse of the special 
## "matrix" returned by 'makeCacheMatrix' above.
