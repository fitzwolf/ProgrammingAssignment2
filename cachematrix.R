##  The goal of these 2 functions is to either solve the inverse matrix of a new 
##  matrix, or if the matrix to be inverted has been seen("cached") before to
##  instead reuse that matrix

## This function makes an inverted matrix and caches it

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <- function(y) { # method for setting the matrix
      x <<- y
      m <<- NULL
    }
    get <- function() { x } # Return the Matrix
    setinverse <- function(inverse) m <<- inverse  # Method to set the inverse of the matrix
    getinverse <- function() { m } # method for getting inverse of matrix
    list(set = set, get = get,    # Return the list of methods
         setmean = setinverse,
         getmean = getinverse)
}
  


## This function solves the matrix inverse or it retrieves
## an existing inverted matrix if it exists

cacheSolve <- function(x, ...) {
    m <- x$getinverse()  # get inverse if it already exists
    if(!is.null(m)) { # confirm it exists and if so, return cached data.
      message("getting cached data")
      return(m)
    }
    # if no cache, compute it, store it and return the result.
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
