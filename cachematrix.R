## Put comments here that give an overall description of what your
## functions do

## This function is like a wrapper. it has a set and get function. Set function
## stores a matrix in cache. Get Function returns the matrix in cache.
## Setinverse stores the inverse of a function in cache. Getinverse returns 
## the inverse of a function stored in cache

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function gets the inverse of a matrix passed as argument to  a function 
## (makeCacheMatrix)

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
  ## Return a matrix that is the inverse of 'x'
}

#This is an example on how to call my functions:
#
#mtx1 <- matrix(c(2,5,3,4),2,2,byrow=TRUE)
#mtx1
#mtxInv<- cacheSolve(makeCacheMatrix(mtx1))
#mtxInv
#mtx1%*%mtxInv

