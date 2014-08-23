## In order to save computing time, this function makes a special cache matrix 
## that can store an original invertible matrix and its inverse. Once the inverse is 
## cached it will not need to be recalculated if it is required again.

## This function takes an invertible matrix as its input. It contains a place to
## store the original matrix (x) and the inverse (inv) and a list of functions to set 
## and retrieve the cached values. The get and getinverse functions expect no input and return the
## original matrix and its inverse, respectively. The set function expects an invertible matrix and
## will reset the cache. The setinverse function expects the inverse of the original matrix, 
## calculated by the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y){
    x <<- y
    inv <<- NULL    
  }
  get = function() {
    x
  }
  setinverse = function(inverse) {
    inv <<- inverse
  }
  getinverse = function() {
    inv
  }
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function takes an invertible matrix as input and checks to see if the inverted
## value of the matrix is stored in the cache. If the data is not cached it sends the
## original matrix to the cache matrix, calculates the inverse of the matrix and sends
## that to the cache matrix as well. Then it returns the inverse.

cacheSolve <- function(x, ...) {
  inv = x$getinverse()
  if (!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  data = x$get()
  inv = solve(data, ...)
  x$setinverse(inv)
  inv
}
