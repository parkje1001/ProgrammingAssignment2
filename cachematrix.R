## Create function to cache the inverse of a matrix (matrix must be invertible)
## Create function to take the inverse of a matrix and assign it to the cache
## Invert a matrix, cache it, then retrieve the inverse from the cached value

## Matrix below can be used to verify functionality
# x = matrix(1:4, 2, 2)


## makeCacheMatrix returns list of functions that will store an inverted matrix
## in the inv variable
makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve checks the cache for the inverted matrix
### If no cache, invert the matrix and cache it
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv = x$getinv()
  
  # Check for cached inverted matrix
  if (!is.null(inv)){ # Cache found, return value from cache
    message("getting cached data")
    return(inv)
  }
  
  # Cache not found, invert the matrix and cache it
  data = x$get()
  inv = solve(data, ...) # Invert the matrix
  x$setinv(inv) # Cache the inverted matrix
  return(inv)
}