## These functions use the 'solve()' function to calculate the inverse
## of a matrix entered into the makeCacheMatrix function.
## The real benefit of these functions is that the output of the 
## makeCacheMatrix is cached or stored for faster retrieval at a later point.

## The makeCacheMatrix function does the "heavy lifting" in that
## it is actually calling the function that will do the calculation
## that we are interested in:  "solve()".  The various list functions are:
##      set:  this stores the matrix that we want to find the inverse of
##      get:  retrieves the stored matrix
##      setinvmatrix:  this calculates the inverse of the stored matrix
##      getinvmatrix:  this retrieves the inverse of the stored matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvmatrix <- function(solve) m <<- solve
  getinvmatrix <- function() m
  list(set = set, get = get,
       setinvmatrix = setinvmatrix,
       getinvmatrix = getinvmatrix)
  
}


## This function leverages the list of functions created above
## in the makeCacheMatrix function.  This function first looks
## for a cached (stored) inverse of the matrix we entered into
## the makeCacheMatrix.  If no cached copy is found then the 
## solve function is called and the results are cached for use
## in the future.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinvmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinvmatrix(m)
  m
}
