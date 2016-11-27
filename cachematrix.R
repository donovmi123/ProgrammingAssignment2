## Put comments here that give an overall description of what your
## functions do

## The following function creates an object "matrix" that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## Set mean to NULL
  m <- NULL
  ## State function to set the vector
  setinverse <- function(y) {
    x <<- y
  ## Set m back to NULL
    m <<- NULL
  }
  ## Return vector x
  get <- function() x
  ##Set the inverse to m
  set <- function(inverse) m <<- inverse
  getinverse <- function() m
  ##list defined functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##The following function calcs the inverse of the matrix created, if the inverse
##has been already calc'ed and is stored in cache it returns the cached value

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
}