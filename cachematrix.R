## Compute inverse of a matrix
## by using cache to store matrix value

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # initialize cache to store the matrix value
  m <- NULL
  # set matrix value
  set <- function(y) {
    x <<- y      # set matrix value
    m <<- NULL   # clear cache
  }
  # get matrix value
  get <- function() x
  # set inverse matrix and cache it
  setinv <- function(inverse) m <<- inverse
  # get inverse matrix from cache
  getinv <- function() m
  # return list with defined functions
  return (list(set=set, get=get, setinv=setinv, getinv=getinv))
}  

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # get matrix in cache
  m <- x$getinv()
  # if matrix in cache then return it
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # otherwise: compute inverse matrix,
  data <- x$get()
  m <- solve(data, ...)
  # cache it,
  x$setinv(m)
  # and return it
  return (m)
}
