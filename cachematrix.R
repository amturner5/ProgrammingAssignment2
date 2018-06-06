## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##this function sets the value of the matrix, 
##gets the value of the matrix, 
##sets the value of the inverse, 
##and gets the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function
##calculates inverse of matrix after checking to see
##if inverse has already been created
##if inverse has been created, it grabs that inverse and skips the computation
##otherwise the inverse is created and stored in the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}