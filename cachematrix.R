## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function creates a matrix-object to be able to cache the inverse of that matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmat <- function(solve) m <<- solve
  getmat <- function() m
  list(set = set, get = get, setmat = setmat, getmat = getmat)
}


## Write a short comment describing this function
#This function chaches an inverted matrix.
#If it is needed, it will be returned without recalculating it.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmat()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setmat(m)
  m
}
