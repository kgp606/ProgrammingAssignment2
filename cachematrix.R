## Put comments here that give an overall description of what your
## functions do
## tHERE are two functions: makeCacheMatrix and cacheSolve. makeCacheMatrix is a function to create a matrix object.
## cacheSolve function takes as input the matrix obtained above and inverses it.  If it has already been calculated before, the function retrieves the inverse from the cache, else computes it.

## Write a short comment describing this function
##The below function has the following steps: set the value of the matrix,get the value of the matrix, set the value of the inverse, get the value of the inverse.


makeCacheMatrix <- function(x = matrix()) {
  a <- NULL
  set <- function(y) {
    x <<- y
    a <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) a <<- inverse
  getinverse <- function() a
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## the below function computes the inverse of the matrix if already not computer before, else retrieves from the cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  a <- x$getinverse()
  if (!is.null(a)) {
    message("getting cached data")
    return(a)
  }
  data <- x$get()
  a <- solve(data, ...)
  x$setinverse(a)
  a
}
B <- matrix(c(1,2,5,4),2,2)
B1 <- makeCacheMatrix(B)

cacheSolve(B1)
