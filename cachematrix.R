## The functions written below are used to cache the inverse of a matrix

## The below function creates a special "matrix" object that can cashe its inverse

makeCacheMatrix <- function(x = matrix()) {
     invrs <- NULL
     set <- function(y) {
             x <<- y
             invrs <<- NULL
}
     get <- function() x
     setInverse <- function(inverse) invrs <<- inverse
     getInverse <- function() invrs
     list(set = set,
          get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}


## The below function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse
## has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse
## from the cache

cacheSolve <- function(x, ...) {
            ## Return a matrix that is the inverse of 'x'
  invrs <- x$getInverse
  if (!is.null(invrs)) {
          message("getting cached data")
          return (invrs)
}
  data <- x$get()
  invrs <- solve(data,...)
  x$setInverse(invrs)
  invrs
}
