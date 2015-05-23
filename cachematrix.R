## The cachematrix.R contains two functions makeCacheMatrix and cacheSolve

## The makeCacheMatrix takes a square matrix as an argument. It stores the matrix and its 
## inverse for future purpose. Both matrix and its inverse can be set and retrieved using the
## respective setter and getter functions. 

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  setMatrix <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function(matrix_inverse) inverse <<- matrix_inverse
  getInverse <- function() inverse
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Return a matrix that is the inverse of 'x'. If inverse is already stored, returns the 
## cached inverse. Else, computes & retrieves the inverse, and stores it for future reference.
## If the matrix is non-invertible, it returns an appropriate message.

cacheSolve <- function(cacheFunctionsList, ...) {
  inverse <- cacheFunctionsList$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- cacheFunctionsList$getMatrix()
  num_rows<-nrow(data)
  num_col<-ncol(data)
  if(num_rows!=num_col){
    message("The matrix is not a square matrix. Hence non-invertible!!")
    return()
  }
  if(det(data)==0){
    message("The determinant of the matrix is zero. Hence non-invertible!!")
    return()
  }
  inverse <- solve(data)
  cacheFunctionsList$setInverse(inverse)
  inverse 
}
