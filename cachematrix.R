## Below are two functions that are used to create a special object that stores a matrix and caches its inverse


## This function creates a special "vector", which is really a list containing functions to
## (1) set the value of the matrix (2) get the value of the matrix
## (3) set the value of the inverse of the matrix and (4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## This function calculates the inverse of the specified matrix x.  However, it first checks to see if the inverse
## has already been calculated.  If so, it "get"s (see above) the inverse from the cache and skips the calclulations. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
  }
  
  
  
