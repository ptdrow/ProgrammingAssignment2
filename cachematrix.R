## These functions create an special matrix and computes the inverse.
## The results are cached for avoiding over calculating the results.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      #Set inverse matrix value as NULL for the current environment
      inv <- NULL
      
      #Function for setting the matrix to compute the inverse
      set <- function(y) {
            x <<- y
            inv <<- NULL ##Delete inverse matrix cached value as the matrix value is changing
      }
      
      #Function for getting the matrix values of the special matrix
      get <- function() x
      
      #Function for caching the inverse matrix values of the special matrix
      setinverse <- function(inverse) inv <<- inverse
      
      #Function for getting the matrix values of the special matrix
      getinverse <- function() inv
      
      #Special matrix:
      list(set = set,
           get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## This function computes the inverse of the special "matrix".
## It retrieves the result from the cache if it has already been calculated.

cacheSolve <- function(x, ...) {
      # Return a matrix that is the inverse of 'x'
      inv <- x$getinverse()
      if(!is.null(inv)) {#Checks if the inverse matrix is already stored
            message("Getting cached data")
            return(inv)
      }
      else {
            data <- x$get() # Gets value of special matrix
            inv <- solve(data, ...) # Compute the inverse matrix
            x$setinverse(inv) # Cache the inverse matrix
            return(inv)
      }
}

