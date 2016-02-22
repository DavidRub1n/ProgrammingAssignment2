## Caching the Inverse of a Matrix 
## These are the two functions used to create a special object that 
## stores a matrix and caches its inverse.

##This function is used to set the value of the vector
##get the value of the vector, set the value of the inverse of the mean
##get the value of the inverse of the mean

makeCacheMatrix <- function(x = matrix()) {
   inverse <- NULL
    set <- function(y) {
      x <<- y
      inverse<<- NULL
    }
    get <- function() x
    setInverse <- function(inverse2)inverse <<- inverse2
    getInverse <- function() inverse
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
  }



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}
 


