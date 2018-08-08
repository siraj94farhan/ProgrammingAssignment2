## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # initializing inverse to null
  i = NULL

  # getMatrix function returns original matrix
   getMatrix <- function() x

  # setMatrix function sets new original matrix
   setMatrix <- function(y) {
    # sets new original matrix data to the environment other than the current environment
     x <<- y
    # initializing inverse matrix to be null
     i <<- NULL
   }

   # returns inverse matrix
   getInverse <- function() i

   # sets inverse matrix to the environment other than the current environment
   setInverse <- function(inverse) i <<- inverse

   # returns all the functions defined above in a list
   list(getMatrix = getMatrix, setMatrix = setMatrix, getInverse = getInverse, setInverse = setInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {

  # get inverse of the matrix
  inverse <- x$getInverse()

  # checks whether inverse of matrix is computed, if not the condition is evaluated to be false
  if(!is.null(inverse)) {

  # if the inverse of matrix is already computed then it's already cached matrix
    print("Got the cached inverse data")
  # cached inverse matrix is returned
    return(inverse)
  }

  # if the inverse of matrix wasn't computed already then control moves to this block

  # gets original matrix
  m <- x$getMatrix()

  # computes inverse of the original matrix
  inverse <- solve(m)

  # sets inverse matrix to the environment other than the current environment
  x$setInverse(inverse)

  # returns computed inverse matrix
  inverse
}
