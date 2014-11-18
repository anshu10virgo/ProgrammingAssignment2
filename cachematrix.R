## Note : It is assumed that the matrix supplied is always invertible.

## This is a special function which creates a special vector and is used to
## set the value of matrix
## get the value of matrix
## set the value of matrix inverse
## get the value of matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  matrixInverse <- NULL
  set <- function(y) {
    x <<- y
    matrixInvers <<- NULL
  }
  get <- function() {x}
  
  setInverse <- function(mInverse) {matrixInverse <<- mInverse}
  getInverse <- function() {matrixInverse}
  
  list(set=set,get=get,
       setInverse=setInverse,
       getInverse=getInverse)
}


## The following function finds the inverse of special matrix created by above function
## It first checks if inverse is cached, if yes then return cached 
## else calculates inverse and set inverse to the calculated for further computations

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matrixInverse <- x$getInverse()
  
  if(!is.null(matrixInverse)) {
    message("Getting cached data")
    return(matrixInverse)
  }
  
  data <- x$get()
  matrixInverse <- solve(data)
  x$setInverse(matrixInverse)
  matrixInverse
}
