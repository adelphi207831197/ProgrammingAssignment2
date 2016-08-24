## 8/23/2016
## R-Programming - Programming assignment 2: Caching the Inverse of a Matrix.
## Write a pair of functions that cache the inverse of a matrix.


## The 'makeCacheMatrix' function creates a special "matrix" object that can cache its inverse.
## Return a list containing functions set, get, setInverse, getInverse.
makeCacheMatrix <- function(x = matrix()) {
  mInverse <- NULL

  # set the value of the new matrix
  set <- function(matrix) {
    x <<- matrix
    mInverse <<- NULL
  }
  
  # get the value of the matrix
  get <- function(){
    x
  } 
  
  # set the value of the inverse
  setInverse <- function(inverse){
    mInverse <<- inverse
  } 
  
  # get the value of the inverse
  getInverse <- function(){
    mInverse
  } 
  
  
  # Prepare list containing a functions set, get, setInverse, getInverse
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## The 'cacheSolve' function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
##
## Computing the inverse of a square matrix can be done with the solve function in R. 
## For example, if X is a square invertible matrix, then solve(X) returns its inverse.
## Assume that the matrix supplied is always invertible.
##
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {

  # Attempt to get the inverse value stored in x.
  inverse <- x$getInverse()
  
  if(!is.null(inverse)) {
    # Cached data was found. We don't have to calculate the inverse here.
    message("getting cached data")
    return(inverse)
  }
  
  # There was no cached data. We have to calculate the matrix inverse.
  myMatrix <- x$get()
  inverse <- solve(myMatrix, ...)
  x$setInverse(inverse)
  
  # Test
  #testAnswer <- myMatrix %*% inverse
  #print(testAnswer)   # Test to see if inverse is correct. We should get 1.
  
  inverse
}