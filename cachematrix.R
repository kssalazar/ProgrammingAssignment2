### Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly. makeCacheMatrix and cacheSolve are functions used to create a special object that stores a matrix and caches its inverse.


## makeCacheMatrix : creates a special matrix object that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                 
  
  ## set value of matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL              
  }
  
  ## get value of matrix
  get <- function() x
  
  ## set value of inverse
  setinv <- function(inverse) inv <<- inverse 
  
  ## get value of inverse
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve : computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated, then the cacheSolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  inv <- x$getinv()
  
  ## If inverse has been already calculated, retrieve from cache
  if (!is.null(inv)){                   
    message("getting cached data")
    return(inv)
  }
  
  ## Otherwise, calculate inverse
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
