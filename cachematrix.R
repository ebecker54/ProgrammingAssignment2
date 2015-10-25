## makeCacheMatrix and cacheSolve work by creating
## a vector of functions that create and stores
## the value of a matrix inverse, then caches its inverse
## then recalls that value from the cache or computes it
## and stores the calculation in the cache

## makeCacheMatrix function creates a list object
## with functions to set and get a matrix value
## then set and get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(
    set = set, 
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
    )
}

## cacheSolve function gets and sets 
##value of matrix inverse from makeCacheMatrix and
##if value is alread calculated, value is retrieved from cache

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
