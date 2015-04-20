## The below functions calculate the inverse of a matrix. In order 
## to save time, calculated inverse matrices are cached and retrieved
## when available, so as not to duplicate calculations.

## The first function, 'makeCacheMatrix' creates a matrix object 
## and can cache the inverse of the matrix object.

makeCacheMatrix <- function(x = matrix()) {
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


## 'cacheSolve' is a function that finds the inverse of the matrix
## from 'makeCacheMatrix'. If previously solved, the inverse is
## retrieved from the cache

cacheSolve <- function(x, ...) {
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
