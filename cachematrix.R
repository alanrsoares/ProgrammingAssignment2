# Creates a cacheable matrix
makeCacheMatrix <- function(x = matrix()) {
  matrixInverse <- NULL
  
  get <- function() x
  
  set <- function(y) {
    x <<- y
    matrixInverse <<- NULL
  }
  
  getInverse <- function() matrixInverse
  
  setInverse <- function(inverse) matrixInverse <<- inverse
  
  list(
    set = set, 
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )  
}

# Try to obtain a previous calculated inverse from the cache if possible
# Otherwise, calculate and cache it
cacheSolve <- function(x, ...) {
  matrixInverse <- x$getInverse()
  
  # Check for cached matrix
  if(!is.null(matrixInverse)) {
    message("Getting cached data.")
    return(matrixInverse)
  }
  
  invertibleMatrix <- x$get()
  # Applies R's native solve() function to calculate the inverted matrix
  matrixInverse <- solve(invertibleMatrix)
  x$setInverse(matrixInverse)
  matrixInverse
}
