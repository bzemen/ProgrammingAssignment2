## makeCacheMatrix creates a special "matrix" object that can cache its inverse
## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above
## and if none is available it computes, caches, and returns it

## makeCacheMatrix creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse_x <- NULL
  set <- function(y) {
    x <<- y
    inverse_x <<- NULL
  }
  get <- function() x
  setinverse<- function(inverse) inverse_x <<-inverse
  getinverse <- function() inverse_x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}


## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse_x <- x$getinverse()
  if (!is.null(inverse_x)) {
    message("getting inverse matrix in cache")
    return(inverse_x)
  } else {
    inv_x <- solve(x$get())
    x$setinverse(inverse_x)
    return(inverse_x)
  }
}
