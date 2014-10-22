## overall
# the following two functions are an adaptation of the mean calculation and caching of a vector by RD Peng that allow to calculate the inversion of a matrix using a prior cached inverse matrix when possible

## cache function
# this function caches the matrix to be inverted and the result of the matrix inversion, potentially releaving the function after this one of the necessety to recalculate an already perfromed inversion. It allows to get and set the matrix and to set and get the inverse matrix.
# not that I continue to use m, but now standing for matrix, not for mean

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

## function that inverts the matrix, but takes the already inverted matrix if cached by the function above

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
