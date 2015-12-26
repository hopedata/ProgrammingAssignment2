## Put comments here that give an overall description of what your
## functions do

## This function creates a list of functions to set/get value of a matrix,
## or to get/set the inverse (solution "soln") of the matrix. setsoln also
## caches the solution, and getsoln retrieves that cached value rather than
## recomputing

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setsoln <- function(soln) inv <<- soln
  getsoln <- function() inv
  list(set = set, get = get,
       setsoln = setsoln,
       getsoln = getsoln)
  
}

## This function interacts with a "CacheMatrix" created by makeCacheMatrix (above)
## It checks to see if the matrix has already been solved, and if so, returns the 
## cached inverse. If there is no cached solution, it calls solve() to get the
## inverse, which it both saves to the cache and returns

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getsoln()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsoln(s)
  s
}
