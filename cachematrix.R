# A pair functions to calculate the inverse of a matrix and store the results in cache.

# Function which caches the inverse of a matrix, and return a list of functions allowing
# the user to get and set both the passed matrix and the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# Returns a matrix that is the inverse of the passed matrix either from cache or 
# by calculating the inverse and storing the result in cache.

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setInverse(i)
  i
}

