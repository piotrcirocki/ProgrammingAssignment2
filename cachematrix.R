

#This function creates matrix that cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x<<- y
    inv<<- NULL
  }
  get <- function() x
  #getting a value of inverse to inv
  setInverseMatrix <- function(inverse) inv <<- inverse
  getInverseMatrix <- function() inv
  #returns a list
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


## Function returns matix inverse of 'x' value
## computes the iverse of matrix from makeCacheMatrix
cacheSolve <- function(x, ...) {

  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  #sets the inverse
  x$setInverse(inv)
  inv
}

#sample call
#cache_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
#cacheSolve(cache_matrix)
#cache_matrix$getInverse()
  
  

