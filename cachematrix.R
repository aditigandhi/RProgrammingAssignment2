#cachematrix.R

## Functions create matrix object that can cache its inverse and can later solve
# (return inverse) the provided matrix either by returning cache if it exists or
# by computing it and saving it in cache to be able to be retreived upon request
# again.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inv) i <- inv
  getinverse <- function() i
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the 
# matrix has not changed), then cacheSolve should retrieve the inverse from the 
# cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setinverse(i)
  i
}