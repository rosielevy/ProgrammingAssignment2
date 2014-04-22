## The two functions below will take a matrix input (assuming it is invertible) and cache the inverse of that matrix in order to save on processing time and computational power.

## The makeCacheMatrix function outputs a function to set and get the values of the matrix and its inverse. This list is a required input for the following function and allows the input matrix to be changes (via the set function) without rerunning the whole of makeCacheMatrix.

makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      set <- function(y) {
            x <<- y
            inverse <<- NULL
      }
      
      get <- function() x
      setinverse <- function(inv) inverse <<- inv
      getinverse <- function() inverse
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## The cacheSolve function first checks to see if the inverse of the input matrix to makeCacheMatrix has already been cached. If not, it calculates and caches the inverse.

cacheSolve <- function(x, ...) {
      inverse <- x$getinverse()
      if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
      }
      
      data <- x$get()
      inverse <- solve(data, ...)
      x$setinv(inverse)
      
      inverse
}
