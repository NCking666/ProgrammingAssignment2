## In order to simplify the calculation of a matrix's inverse, design 2 functions,
## one is to create a vector of functions, the other one is to check the necessity
## of calculation.


## To get a special list which in fact contains four functions

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## To calculate the inverse of the matrix. If that inverse has been exists,
##just return the value.

cacheSolve <- function(x) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
