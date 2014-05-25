## makeCacheMatrix function creates and saves a cached value
## of an inverted matrix.  cacheSolve function returns
## the cached value of the inverted matrix if it exists.  If
## it does not exist, cacheSolve function will calculate and
## cache the inverse.

## makeCacheMatrix creates and saves a cached value of an
## inverted matrix, using 'solve'.

makeCacheMatrix <- function(x = matrix()) {
     i <- NULL
     set <- function(y) {
          x <<- y
          i <<- NULL
     }
     get <- function() x
     setmatrix <- function(solve) i <<- solve
     getmatrix <- function() i
     list(set = set, get = get,
          setmatrix = setmatrix,
          getmatrix = getmatrix)
}


## cacheSolve function solves the inverted matrix, if it exists.
## If it does not exist, it will calculate and cache the inverse
## using function 'solve'.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     i <- x$getmatrix()
     if(!is.null(i)) {
          message("getting cached data")
          return(i)
     }
     data <- x$get()
     i <- solve(data)
     x$setmatrix(i)
     i  
}
