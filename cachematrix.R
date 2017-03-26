## The functions below create a matrix object that can cache its inverse and also
## compute the value of the matrix inverse

## The makeCacheMatrix below creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setinversematrix <- function(solve) I <<- solve
  getinversematrix <- function() I
  list(set = set, get = get,
       setinversematrix = setinversematrix,
       getinversematrix = getinversematrix)
}


## The cacheSolve function below calculates the inverse of the special "matrix'
## returned by the makeCacheMatrix above. If the function is called more than
## once, it returns the inverse of the matrix from cache along with the message 
## 'getting cached data'.

cacheSolve <- function(x, ...) {
  I <- x$getinversematrix()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  I <- solve(data, ...)
  x$setinversematrix(I)
  I
  }
