## Put comments here that give an overall description of what your
## functions do

## this function sets process for handling storage 
## and association of variables/matrices
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {  
    x <<- y    ##changes input for main function(makeCacheMatrix) from x to y
    m <<- NULL ##resets m to null so it is not used in old functions
  }
  get <- function() x  ##returns matrix
  setinverse <- function(solve) m <<- solve ##stores value of input variable
  ## function "solve" is used to calculate matrix inverse
  getinverse <- function() m ##obtains value of input
  list(set = set, get = get,       ##creates association to functions
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function checks for variables and matrix and solves for inverse
cacheSolve <- function(x, ...) {
  m <- x$getinverse()  ## gets value
  if(!is.null(m)) {  ## checks value of m
    message("getting cached data")  ## if not null returns m to use and
    return(m)                       ## notifies user
  }
  data <- x$get()   ##obtains matrix
  m <- solve(data, ...)  ##performs actual calculation
  x$setinverse(m) ##sets value
  m      ##displays value and returns matrix tha is inverse of 'X'
}
          ## Return a matrix that is the inverse of 'x'


