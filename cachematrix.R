#These functions are helping to create a matrix-object and cache the inverse
#of the matrix since the computation could be take too long time.

#This function creates a matrix-object to be able to cache the inverse of a matrix.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmat <- function(solve) m <<- solve
  getmat <- function() m
  list(set = set, get = get, setmat = setmat, getmat = getmat) #return the "special" matrix
}

#This function chaches an inverted matrix.
#If it is needed, it will be returned without recalculating it.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmat()
  if(!is.null(m)) { #if the matrix is not null
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setmat(m)
  m
}
