makeCacheMatrix <- function(x=matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setValue <- function(matrix) m <<- solve(matrix)
  getValue <- function() m
  list(set = set, get = get, 
       setValue = setValue, 
       getValue = getValue)
}

cacheSolve <- function(x) {
  m <- x$getValue()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setValue(data)
  m
}