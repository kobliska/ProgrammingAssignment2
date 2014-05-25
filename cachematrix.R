makeCacheMatrix <- function(x = matrix()) {
  ## This function creates a special "matrix" object that can cache its inverse.
  ## set is a fn that allocates storage for the input data.
  ## get retrieves that data.
  ## setInvMat defines storage as well as permits definition of matrix inverse.
  ## getInvMat retrieves stored inverse matrix or says is NULL(doesn't exist).

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInvMat <- function(InvMat) m <<- InvMat
  getInvMat <- function() m
  list(set = set, get = get,
       setInvMat = setInvMat,
       getInvMat = getInvMat)
}
cacheSolve <- function(x, ...) {

  ##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
  ##  If the inverse has already been calculated (and the matrix has not changed), 
  ##  then the cachesolve should retrieve the inverse from the cache.
  ## cacheSolve returns a matrix that is the inverse of 'x' using the solve fn.
  
  m <- x$getInvMat()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInvMat(m)
  m
}
