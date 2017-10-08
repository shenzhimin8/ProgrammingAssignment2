# Zhimin Shen 10/08/2017
# Matrix inversion is usually a costly computation 
# and there may be some benefit to caching the inverse 
# of a matrix rather than compute it repeatedly (there 
# are also alternatives to matrix inversion that we will not discuss here). 
# Your assignment is to write a pair of functions that cache the inverse of a matrix.
#

#makeCacheMatrix: 
#This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(m = matrix()) {
  
  if (!is.matrix(m)) {stop ("not a matrix")}
  if (!is.numeric(m)) {stop ("not a numeric matrix")}
  
  inv <- NULL
  
  set <- function(y) {
    m <<- y
    inv <<- NULL
  }
  
  get <- function() m
  
  setinv <- function(p) inv <<- p
  
  getinv <- function() inv
  
  fnlist<-list(set = set, 
               get = get,
               setinv = setinv,
               getinv = getinv)
  
  return(fnlist)
}

#cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  
  inv <- solve(data, ...)
  
  x$setinv(inv)
  
  inv
}

mtr1 <- matrix( c(5, 1, 0,
                  3,-1, 2,
                  4, 0,-1
), nrow=3, byrow=TRUE)

t<-makeCacheMatrix(mtr1)

cacheSolve(t)

rm(list=ls())




