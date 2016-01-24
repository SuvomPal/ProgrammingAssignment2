

makeCacheMatrix <- function(x = matrix()) {     ## setting the matrix
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x                           ## getting the matrix
  setinverse <- function(inverse) m <<- inverse ## setting the value of the inverse
  getinverse <- function() m                    ## getting the value of the inverse
  list(set = set, get = get,                    ## list containing the funcions
       setinverse = setinverse,
       getinverse = getinverse)
}



cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {                ## checking if m is null or not.
      message("getting cached data") ## If non null the value of m is cached.
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)            ## inverse is calcullated.
    x$setinverse(m)
    m
  }

