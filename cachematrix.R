## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL # this is where the result of inversion is stored
  set <- function(y) {
    x <<- y
    xinv <<- NULL 
  }
  
  get <- function() x # return the input matrix
  setInv <- function(inv) xinv <<- inv # set the inversed matrix
  getInv <- function() xinv # return the inversed matrix

  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInv() # get the inversed matrix from object x
  # it will be null if uncalculated, remember the first line "xinv <- NULL" in the previous function
  if(!is.null(m)) { # if the inversion result is there
    message("getting cached data")
    return(m) 
  }
  data <- x$get()
  m <- solve(data) 
  x$setInv(m) 
  m # return the solved result
}
