## Pair of functions that cache the inverse of a matrix.

## Function creates variable 'x' with matrix from parameter and empty 'inv' for matrix inverse
## in parent environment

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) inv <<- solve
  getsolve <- function() inv
  list(set = set, 
       get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## First time function calculates inversion by solve().
## Second time previously calculated value is used.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getsolve()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setsolve(inv)
  inv
}
