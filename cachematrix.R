## Function to store the result of a matrix inversion

makeCacheMatrix <- function(x = matrix()){
  i <- NULL
  set <- function(matrix) {
    m <<- matrix
    i <<- NULL
  }
  get <- function() {
    m
  }
  setInverse <- function(inverse) {
    i <<- inverse
  }
  getInverse <- function() {
    i
  }
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## Function to compute the inverse of the matrix stored in makeCacheMatrix
      
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data) %*% data
  x$setInverse(m)
  ## Return a matrix containing the inverse of "x"
  m
}