## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(y)
  {
    x <<- y
    inv_x <<- NULL
  }
  get <- function()x
  setinverse <- function(solve) inv_x <<- solve
  getinverse <- function() inv_x
  list( set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if ( !is.null(i))
  {
    message(" getting cached inverse matrix")
    return(i)
  }
  else
  {
    dat <- x$get()
    i <- solve(dat, ...)
    x$setinverse(i)
    i
  }
}
