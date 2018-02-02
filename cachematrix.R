## Matrix inverse calculation and caching

## Takes a matrix and returns a set of functions which allow to 
## cache inverse calculation.
## It increases the performance as it saves a lot of time to calculate 
## inverse if it's been already proceed once.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  ## Set the matrx value
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  ## Get the matrix value
  get <- function() x
  ## Set the cache inverse value
  setinv <- function(inv) inverse <<- inv
  ## Get the cached inverse value
  getinv <- function() inverse
  # Return the vector of functions for getting a special matrix
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Returns the inverse from cache from a matrix created by makeCacheMatrix
## Else find the inverse and store in the cache

cacheSolve <- function(x) {
  ## Check the cache for inverse
  inv <- x$getinv()
  ## If found, return it
  if(!is.null(inv)) {
    return(inv) 
  }
  ## Else find the inverse for raw matrix
  data <- x$get()
  inv <- solve(data)
  ## Store inverse in cache and return it
  x$setinv(inv)
  inv
}

m <- makeCacheMatrix(matrix(rnorm(25),5,5))
cacheSolve(m)    ## This call calculates the inverse and store in cache
cacheSolve(m)    ## The second call retrieves inverse from cache