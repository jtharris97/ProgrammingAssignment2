## The following functions take adantage of the lexical scoping in R to 
## calculate and cache the inverse of a matrix. The first function creates
## a special "matrix" object that can cache its inverse. The second function
## calculates the inverse, but first checks to see if the inverse has
## already been calculated, and if it has, the function gets the inverse from the
## cache and skips the computation.

## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## Computes the inverse, if inverse already calculated then will retrive the
## inverse from the cache

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
