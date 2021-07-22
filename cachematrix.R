## My functions below are used to create a special object that stores a matrix
## and cache's its inverse.


# The first function creates a special matrix object that can cache its inverse.
# It creates a list of functions that set the value of the matrix, get the value
# of the matrix, set the value of the inverse of the matrix, and get the value
# of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
              i <- NULL
              set <- function(y) {
                x <<- y
                i <<- NULL
              }
              get <- function() x
              setinverse <- function(solve) i <<- solve
              getinverse <- function() i
              list(set = set, get = get,
                   setinverse = setinverse,
                   getinverse = getinverse)
}

# The second function calculates the inverse of the special matrix created
# with the above function.  First, it checks to see if the inverse has already
# been calculated.  If so, it gets the inverse from the cache and skips the
# computation.  Otherwise, it calculates the inverse of the special matrix and
# sets the value of the inverse in the cache via the setinverse function.


cacheSolve <- function(cs, ...) {
              i <- cs$getinverse()
                if(!is.null(i)) {
                  message("getting cached data")
                  return(i)
                }
              data <- cs$get()
              i <- solve(data,...)
              cs$setinverse(i)
              i
}
