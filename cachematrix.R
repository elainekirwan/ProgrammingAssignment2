## R Programming Assignment 2 
## The 2 functions makeCacheMatrix() and cacheSolve() can be used together 
## to create a square matrix and to cache and calculate its inverse.

## `makeCacheMatrix` creates an invertible matrix, which is
##  a list containing a function to
##    1.  set the value of the matrix
##    2.  get the value of the matrix
##    3.  set the value of the inverse
##    4.  get the value of the inverse

## makeCacheMatrix() creates an invertible matrix object, which is a 
## list of functions used by cacheSolve() to get and set an inverted 
## matrix in cache.

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y) {
      x <<- y
      m <<- NULL
   }
   get <- function() x
   setInv <- function(inverse) m <<- inverse
   getInv <- function() m
   list(set = set, 
        get = get,
        setInv = setInv, 
        getInv = getInv)
}


## cacheSolve() computes the inverse of the matrix returned by 
## makeCacheMatrix() above. If the inverse has already been calculated
## (and the matrix has not changed), then cacheSolve() should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
   m <- x$getInv()
   if(!is.null(m)) {
      message("getting cached data")
      return(m)
   }
   data <- x$get()
   m <- solve(data, ...)
   x$setInv(m)
   m
}
