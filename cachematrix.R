## makeCacheMatrix(): creates a special “matrix” object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
      # @x: a square invertible matrix
      # return: a list containing functions to
      #              1. set the matrix
      #              2. get the matrix
      #              3. set the inverse
      #              4. get the inverse
      #         this list is used as the input to cacheSolve()
  
  i <- NULL
  set <- function(y) {
    
      # use <<- to assign a value to an object in an environment 
      # different from the current environment.
    
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


## cacheSolve(): computes the inverse of the “matrix” returned by 
## makeCacheMatrix(). If the inverse has already been calculated and 
## the matrix has not changed, it’ll retrieves the inverse from the cache directly.

cacheSolve <- function(x, ...) {
      # @x: output of makeCacheMatrix()
      # return: inverse of the original matrix input to makeCacheMatrix()
  
  i <- x$getinverse()
  
      # if the inverse has already been calculated
  
  if(!is.null(i)) {
    
      # get it from the cache and skips the computation.
    
    message("getting cached data")
    return(i)
  }
  
    # otherwise, calculates the inverse 
  data <- x$get()
  i <- solve(data, ...)
  
    # sets the value of the inverse in the cache via the setinv function.
  x$setinverse(i)
  
    #print i / return i
  i
}
