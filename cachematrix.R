
## This function, makeCacheMatrix, takes a matrix and returns a list containing four functions to
## set the matrix
## get the matrix
## set the inverse of the matrix
## get the inverse of the matrix
## 
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function, cacheSolve, takes a list as created by makeCacheMatrix, and returns the inverse of the associated matrix.
## First time the inverse is caculated it is cached and subsequent calls will return the cached value. 
##
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i  
}
