## Put comments here that give an overall description of what your
## functions do

## Returns a special 'matrix' with get, set, getinverse, and setinverse functions. 
##Also, when you initalize this special matrix, it will automatically set to the input 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  set(x)
  
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse = getinverse)
}


## Returns the inverse of a special matrix x.  X is assumed to be inversible and square.
##If x already has cached the inverse, then just returns the cached value

cacheSolve <- function(x, ...) {
  s <- x$getinverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(s)
  s}

