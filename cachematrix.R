# This seems like OOP in R.  The first part creates an object that's a list
# That knows how set its own value, get its own value, and also get and set its own
# solved values.



# Creates an object that stores the value of the matrix and also knows how to 
# Get, set its own value and also get and set its own inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

# CacheSolve takes an instance, or multiple instances of the objects created by the function 
# above, if solve has not been run on this object and it has not been stored, it stores
# the result of the solve operation.  Otherwise it retrieves it from cache and returns it.
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
