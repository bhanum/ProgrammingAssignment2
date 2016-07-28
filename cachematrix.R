## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(m = matrix()) {
  inv <- NULL
  set <- function(y) {
    m <<- y
    inv <<- NULL
  }
  get <- function() m
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(set = set,
       get = get,
       setInv = setInv,
       getInv = getInv)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated and 
## matrix has not changed, then it should retrieve the inverse from cache.

cacheSolve <- function(m, ...) {
  ## Return a matrix that is the inverse of 'm'
  inv <- m$getInv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- m$get()
  inv <- solve(mat, ...)
  m$setInv(inv)
  inv
}