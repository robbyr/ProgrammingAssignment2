## makeCacheMatrix and cacheSolve are functions that together create a matrix, compute
## the inverse of that matrix and stores the result
##
## EXAMPLE USAGE:
## > a <- makeCacheMatrix(matrix(c(6,2,33,24)2,2))
## > a$get()
##      [,1] [,2]
## [1,]    6   33
## [2,]    2   24
## > cacheSolve(a)
##             [,1]        [,2]
## [1,]  0.30769231 -0.42307692
## [2,] -0.02564103  0.07692308
## > cacheSolve(a)
## Fetching the cached inverse
## [,1]        [,2]
## [1,]  0.30769231 -0.42307692
## [2,] -0.02564103  0.07692308


## makeCacheMatrix sets up the parameters needed for cacheSolve to compute the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  ## initalize inverse
  inverse <- NULL
  ## set allows changes to be made to matrix
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  ## get displays the matrix
  get <- function() x
  ## setmatrix sets the inverse matrix
  setmatrix <- function(solve) inverse <<- solve
  ## getmatrix get the inverse matrix
  getmatrix <- function() inverse
  ## builds the function list
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

## cacheSolve takes matrix set in makeCacheMatrix then computes and returns inverse
cacheSolve <- function(x, ...) {
  ## set inverse to be the inverse of the matrix
  ## returns NULL if the inverse has not been computed yet
  inverse <- x$getmatrix()
  ## checks to see if the inverse has been computed
  ## if so, return the previously computed inverse and exit
  if(!is.null(inverse)) {
    message("Fetching the cached inverse")
    return(inverse)
  }
  ## set mymatrix to be original matrix
  mymatrix <- x$get()
  ## compute inverse
  inverse <- solve(mymatrix, ...)
  ## set argument x to inverse
  x$setmatrix(inverse)
  ## return the computed inverse
  inverse
}