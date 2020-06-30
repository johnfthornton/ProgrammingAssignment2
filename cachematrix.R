## The makeCacheMatrix function creates a special matrix
## object thaat can cache its inverse
## function assumes that an invertible matrix is passed to it

## The cacheSolve function computes the inverse of the
## special matrix returned by the makeCacheMatrix above
## If he inverse has already been calculated and the matrix
## is unchanged, then the cacheSolve should retrieve the inverse
## from the cache

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y = matrix()) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) m <<- Inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
  
}
