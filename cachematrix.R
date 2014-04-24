## These functions allow the caching of the inverse of a matrix. Inverting
## a matrix can be computationally costly, so these functions allow such
## an inversion to be cached for later access.

## To use these functions, first pass an invertible matrix to makeCacheMatrix
## and asssing the output to a variable. Then pass that variable to cacheSolve,
## which will return the inverted matrix.
## Example:
##  Y
## [,1] [,2] [,3]
## [1,]    1    1    2
## [2,]    1    2    3
## [3,]    1    2    4
## L = makeCacheMatrix(Y)
## cacheSolve(L)
## [,1] [,2] [,3]
## [1,]    2    0   -1
## [2,]   -1    2   -1
## [3,]    0   -1    1

## This function makes a special 'matrix' that is in actuality a list that allows
## setting and getting the value of the matrix and setting and getting the inverse
## of that matrix.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function inverts the matrix created with the function above.
## Before inverting, it checks to see whether that inversion has been cached,
## in which case it returns the cached inversion rather than inverting the matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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

