## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setImatrix <- function(Imatrix) m <<- Imatrix
  getImatrix <- function() m
  list(set = set, get = get,
       setImatrix = setImatrix,
       getImatrix = getImatrix)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m <- x$getImatrix()
  if(!is.null(m)) {
    message("getting cached matrix data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setImatrix(m)
  m
}
