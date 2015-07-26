## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix
## rather than compute it repeatedly.
## Following two functions do cache the inverse of a matrix.


## This function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to 
##  1.set the value of the vector
##  2.get the value of the vector
##  3.set the value of the mean
##  4.get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function calculates the inverse of the special "matrix" created with the above function, makeCacheMatrix.
## It first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and
## skips the computation. Otherwise, it calculates the inverse of the data and sets the inverse in the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
