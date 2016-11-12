## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x<<- y
    inv<<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(inverse) inv <<- inverse
  getInverseMatrix <- function() inv
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  #get inverse of a matix
  inv <- x$getInv()
  if(!is.null(inv)){
    message('getting cached data')
    return(inv)
  }
  matrix <- x$get()
  inv <- mean(matrix,...)
  x$setInverseMatrix(inv)
  inv
  
}

