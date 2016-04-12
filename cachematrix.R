## Functions create a special "matrix" capable of taking a numeric matrix and caching its inverse,
## and either return the cached value, if already computed, or compute the inverse from scratch.

## Takes a matrix and creates a special "matrix" containing list
## of four functions that set and get matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function(){
    x
  }
  setInverse <- function(inverse){
    i <<- inverse
  }
  getInverse <- function(){
    i
  }
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Takes special "matrix", tests whether cache is null.
## If cache is not null, returns inverse and terminates functions, else calculates inverse.

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}