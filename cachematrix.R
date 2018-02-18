

## makeCacheMatrix allows the user to enter matrix x into the function and cache the 
## inverse of that matrix as m. It sets m to NULL in the parent environment
## before solve is applied so that any 
## cached, inverted matrices are cleared before caching a new inverted matrix as m.
## This function also uses the <<- operator 
## during the setSolve step to ensure that value of m is set in the parent environment
## and not the child environment

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


##cacheSolve looks to see if m has been cached and calls up cached data if that is
## the case. If m has not been cached and is therefore NULL, then cacheSolve carries out 
## the steps necessary to produce the m, the inverse of matrix x.

cacheSolve <- function(x, ...) {
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m
}
