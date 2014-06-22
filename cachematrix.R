## Caching the Inverse of a Matrix


## Creates a special "matrix" object that can cache its inverse
## It contains functions to set/get the matrix and slo set/get the inverse of the matrix
makeCacheMatrix <- function(x = numeric()) {
  i <- NULL
  
  ## Set the values of the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## Get the value of the matrix
  get <- function() x
  
  ## Set the value of the inverse
  setinverse <- function(inverse) i <<- inverse
  
  ## Get the value of the inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Computes the inverse of the special "matrix" (from makeCacheMatrix).  
## If the inverse has previously been calculated, then the inverse will be retrieve from the cache.
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  
  ## Return a matrix that is the inverse of 'x'
  i
}
