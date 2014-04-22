#Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather
#than computing it repeatedly. The following pair of functions cache the inverse of a matrix.

## makeCacheMatrix: A function to add the above mentioned feature to matrix.
## inputs
##    x: a square invertible matrix
## return: a list containing functions to
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
##         this list is used as the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
  matrixInverse <- NULL
  
  set = function(y) {
    # use `<<-` to assign a value to an object in an environment different from the current environment.
    x <<- y
    matrixInverse <<- NULL
  }
  
  get <- function() x
  setinv <- function(inverse) matrixInverse <<- inverse
  getinv <- function() matrixInverse
  
  list(
    set = set,
    get = get,
    setinv = setinv,
    getinv = getinv
  )
}


## x: output of makeCacheMatrix()
## return: inverse of the original matrix input to makeCacheMatrix()

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  # if the inverse has already been calculated
  if (!is.null(inv)) {
    # get it from the cache and skips the computation.
    message("getting cached data")
    return(inv)
  }
  
  # otherwise, calculates the inverse
  matrixdData <- x$get()
  inv <- solve(matrixdData, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv)
  inv
}
