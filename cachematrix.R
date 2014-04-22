## This file provides function that can be used to cache the result of Solution of equation 
## (ie solve {base} function from base R package)
## See R Documentation for details of solve function
## Note : To calculate inverse of matrix do not pass b (right-hand side(s) of the linear system)
## This function creates a special "matrix" object that can cache its solve result

## Example :
## hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
## A <- hilbert(4)
## mat = makeCacheMatrix(A)
## cacheSolve(mat)

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) inverse <<- solve
  getsolve <- function() inverse
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}


## This function computes the result of solve for the special "matrix" returned by makeCacheMatrix above. 
## If the result has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the result from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inverse <- x$getsolve()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data,...)
  x$setsolve(inverse)
  inverse
}


