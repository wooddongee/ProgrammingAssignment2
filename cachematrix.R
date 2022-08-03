## Comprised of two functions, "makeCacheMatrix" and "cacheSolve"

## makeCacheMatrix is a list of functions, which sets the value and inverse of the matrix and get them

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) I <<- inverse
  getinverse <- function() I
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## And then this cacehSolve gets the inverse from the cache if it exists. If not, it calculates it.

cacheSolve <- function(x, ...) {
  I <- x$getinverse()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  I <- solve(data, ...)
  x$setinverse(I)
  I
}
