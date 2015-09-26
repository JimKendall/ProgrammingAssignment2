## This function creates a special "matrix" object that can cache its inverse.
## 

makeCacheMatrix <- function(x = matrix()) {
  
## set variables
  
  invert <- NULL
  set <- function(y) {
    x <<- y
    invert <<- NULL
  }
  
## set up inversion and cache
  
  get <- function() x
  setInvert <- function(inverse) invert <<- inverse
  getInvert <- function() invert
  list(set = set, 
       get = get,
       setInvert = setInvert,
       getInvert = getInvert)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  invert <- x$getInvert()
  
## Process solve or use cache
  
  if(!is.null(invert)) {
    message("getting data from cache")
    return(invert)
  }
  data <- x$get()
  invert <- solve(data, ...)
  x$setInvert(invert)
  invert
}
## Return a matrix that is the inverse of 'x' completed
