## In this programming exercise we use two functions to create a special matrix
## which can store its inverse in the cache and a function that either calculates the
## inverse of the matrix data or gets it from the cache if it was allready calculated.

## This function creates a special matrix that is a list of functions and
## can store its inverse in the cache
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  # with the set function one can change the 
  # matrix data in an existing "special matrix"
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # function to get the matrix data from the special matrix
  get <- function() x
  
  # function to save the inversed matrix in the cache
  setinverse <- function(inversed) inv <<- inversed
  
  # function to get the inversed matrix from the cache;
  # if inverse is not yet calculated returns NULL
  getinverse <- function() inv
  
  # return of makeCacheMatrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function calculates an inverse or gets it from the cache
## of a special matrix x generated with makeCacheMatrix 

cacheSolve <- function(x, ...) {
  # get inverse from the cache of special matrix x      
  inv <- x$getinverse()
  
  # if inverse matrix was already calculated return the inverse from the cache
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # get the data from special matrix x
  data <- x$get()
  # calculate the inverse of the matrix
  inv <- solve(data, ...)
  # store the calculated inverse matrix in the cache of special matrix x
  x$setinverse(inv)
  
  ## Return the calculated inverse matrix
  inv
}
