## This function creates a special "matrix" object


makeCacheMatrix <- function(x = matrix()) {
  
  invMatr <- NULL
  set <- function(y) {
    x <<- y
    invMatr <<- NULL
  }
  
  get <- function(){
    x
  }
  
  setinverse <- function(inverse) {
    invMatr <<- inverse
  } 
  
  getinverse <- function() {
    invMatr
  }
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## This function computes the inverse of the matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  invMatr <- x$getinverse()
  if(!is.null(invMatr)) {
    message("getting cached data")
    return(invMatr)
  } 
  data <- x$get()
  invMatr <- solve(data, ...)
  x$setinverse(invMatr)
  invMatr
}
