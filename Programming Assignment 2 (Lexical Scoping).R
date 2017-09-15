setwd("C:/Users/svzhpf/Desktop/R course/")
getwd()

## This function make the matrix##

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## solution funtion

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

# Testing Functions

matrix_1 <- makeCacheMatrix(matrix(1:4, 2, 2))
matrix_1$get()

matrix_1$getInverse()

cacheSolve(matrix_1)

matrix_1$getInverse()

matrix_1$set(matrix(c(2, 2, 1, 4), 2, 2))

matrix_1$get()

matrix_1$getInverse()

cacheSolve(matrix_1)

matrix_1$getInverse()



