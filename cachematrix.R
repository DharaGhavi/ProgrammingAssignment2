## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## x is a square invertible matrix
  ## As like in the example, 1. set the matrix values,
  ##                         2. Get the matrix values,
  ##                         2. Set the Inverse of matrix values,
  ##                         4. Get the Inverse of matrix values,
  
  inverse <- NULL
   set <- function (y) {
     x<<- y
     inverse <<- NULL
   }
   
   get = function() x
   setinverse = function(inverseofx) inverse <<-inverseofx
   getinverse = function() inverse
   
   list (set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}
  
  
  
  ## Write a short comment describing this function
  
  cacheSolve <- function(x, ...) {
    ## x is the output of the makecachematrix function
    ## coding to get the original matrix from the inverse matrix from the last transformation - X -> X^-1 -> X
   inverse = x$getinverse()
   ## assigning the inverse matrix from the previously stored values
   if(!is.null(inverse)){     
     message ("getting catched data")
     ## if the values are available, return the inverse matrix
     return (inverse)
   }
   ## else calculate the inverse matrix
   data <- x$get()
   inverse <- solve(data, ...)
   x$setinverse(inverse)
   inverse
  }
