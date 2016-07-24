makeCacheMatrix <- function(x = matrix()) {
  # set & the value of the matrix
  # set & get the value of the inverse  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  # if it is already calculated, gets it from the cache
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

# test case
# a<-makeCacheMatrix()
# a$set(matrix(1:4,2,2))
# cacheSolve(a)