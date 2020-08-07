## These two functions were designed to cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse
## assuming that the matrix supplied is always invertible.

makeCacheMatrix <- function(x = matrix()) {
   invmtrx <- NULL
   set <- function(y) {
      x <<- y
      invmtrx <<- NULL
   }
   get <- function() x
   setinverse <- function(inverse) invmtrx <<- inverse
   getinverse <- function() invmtrx
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cacheSolve should retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
   invmtrx <- x$getinverse()
   if(!is.null(invmtrx)) {
      message("geting cached data")
      return(invmtrx)
   }
   data <- x$get()
   invmtrx <- solve(data, ...)
   x$setinverse(invmtrx)
   invmtrx    ## Return a matrix that is the inverse of 'x'
}
