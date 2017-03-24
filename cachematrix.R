## The following function creates a vector with a list of functions
## that allows to obtain (with the function getmatrixinverse()) the inverse of the matrix passed as argument
## and set it in the cache (with the function setmatrixinverse())

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
           x <<- y
           m <<- NULL
    }
    get <- function() x
    setmatrixinverse <- function(solve) m <<- solve
    getmatrixinverse <- function() m
    list(set = set, get = get,
    setmatrixinverse = setmatrixinverse,
    getmatrixinverse = getmatrixinverse)
}



## The following function evaluate if the inverse matrix is already calculated
## If so, it prints a message and returns the inverse matrix from the cache 
## If not, it calculates the inverse matrix of the matrix passed as argument, set it in the cache and return its value

cacheSolve <- function(x, ...) {
      m <- x$getmatrixinverse()
      if(!is.null(m)){
            message("getting cached inverse of the matrix")
            return(m)
      }
      originalmatrix <- x$get()
      m <- solve(originalmatrix, ...)
      x$setmatrixinverse(m)
      m
}
