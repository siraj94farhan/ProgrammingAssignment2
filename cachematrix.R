## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i = NULL
   getMatrix <- function() x
   setMatrix <- function(y) {
     x <<- y
     i <<- NULL
   }

   getInverse <- function() i
   setInverse <- function(inverse) i <<- inverse

   list(getMatrix = getMatrix, setMatrix = setMatrix, getInverse = getInverse, setInverse = setInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()

  if(!is.null(inverse)) {
  print("Got the cached inverse data")
  return(inverse)
  }

  m <- x$getMatrix()
  inverse <- solve(m)
  x$setInverse(inverse)

  inverse
}
