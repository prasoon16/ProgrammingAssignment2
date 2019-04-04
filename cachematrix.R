## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## return list of function which will be used in cacheSolve
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  get <- function() x
  set <-function(y) {
    x <<- y
    inv <<- NULL
  }
  getInv <-function() inv
  setInv <- function(ninv) inv <<- ninv
  list(get = get, set = set, getInv = getInv, setInv = setInv)
}


## Write a short comment describing this function
##return inv using function list returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInv()
  if (!is.null(i)) {
    print("Getting inverse from cache")
    i
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInv(i)
  i
}
