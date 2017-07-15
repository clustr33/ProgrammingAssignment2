## Initialize a matrix object with cached inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  ## inv attribute to store the inverse matrix. It is not calculated until needed.
  inv <- NULL
  
  ## Getters and setters for the matrix and the inverse matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Method to get the inverse matrix of the cached matrix
## If the inv attribute is set (is.null() != TRUE), the inverse will be
## returned from the memory. Otherwise, it will be calculated and updated
## to the inv attribute.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("Getting cached inverse matrix")
    return(inv)
  }
  message("Calculating inverse matrix")
  mat <- x$get()
  inv <- solve(mat)
  x$setinv(inv)
  return(inv)
}


## Test case

## Initialize a 3x3 matrix with random numbers and make a Cache Matrix object
## > my_mat <- matrix(runif(9), 3, 3)
## > cachemat <- makeCacheMatrix(my_mat)

## Check if inverse matrix is set:
## > cachemat$getinv()
## NULL

## Calculate the inverse matrix for the first time
## > cacheSolve(cachemat)
## Calculating inverse matrix
## [,1]       [,2]       [,3]
## [1,] -0.5541711  0.8953160  0.3409795
## [2,]  0.9496297  0.7151222 -1.5915836
## [3,]  0.3841890 -0.6926966  1.4029287

## Check if the inverse matrix is set:
## > cachemat$getinv()
## [,1]       [,2]       [,3]
## [1,] -0.5541711  0.8953160  0.3409795
## [2,]  0.9496297  0.7151222 -1.5915836
## [3,]  0.3841890 -0.6926966  1.4029287

## Solve again; this time from memory
## > cacheSolve(cachemat)
## Getting cached inverse matrix
## [,1]       [,2]       [,3]
## [1,] -0.5541711  0.8953160  0.3409795
## [2,]  0.9496297  0.7151222 -1.5915836
## [3,]  0.3841890 -0.6926966  1.4029287