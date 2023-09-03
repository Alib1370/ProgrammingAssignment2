
## These functions create an object to stores a matrix and caches its inverse.
## "makeCacheMatrix" creates a special matrix, including a function to:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
  set <- function(y) {
          x <<- y
          m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The other function calculates the inverse of the matrix returned by makeCacheMatrix. 
##If the inverse exists, cacheSolve retrieve the inverse from the cache.
## The process is the same is cachemean example.

cacheSolve <- function(x, ...) {
         m <- x$getinverse()
  if (!is.null(m)) {
          message("getting cached data")
          return(m)
  }
  data <- x$get()
  m<- solve(data, ...)
  x$setinverse(m)
  m
}
