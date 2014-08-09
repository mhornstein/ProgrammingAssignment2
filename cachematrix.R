## This file represents the CacheMatrix functionality:
## *makeCacheMatrix creates a a data structure holding a matrix
## and it corresponding inverse matrix.
## *cacheSolve return a given CacheMatrix's inverse. if none was
## calculated before- it calculates it and updates the CacheMatrix data
## Assumption: all the given matrix are invertable.

## A data structure that holds a matrix and its inverse. 
## provides 4 functions:
## *get- returns the matrix
## *set- sets the matrix to the given arguments
## *getinverse- return the inverse matrix held by the data structure
## *setinverse- sets the inverse given the required argument

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## given a CacheMatrix x, checks if its inverse was calculated.
## if it was- returned it.
## if not- update the CacheMatrix with its calculated inverse. and than-
## returns it

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
