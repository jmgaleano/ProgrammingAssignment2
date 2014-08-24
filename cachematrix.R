# The function makeCacheMatrix will return a list of functions
# The puspose of it is to store a martix plus a cached value of the inverse  
# matrix. has the following functions:
# * setMatrix      set the value of a matrix
# * getMatrix      get the value of a matrix
# * cacheInverse   get the cahced value (inverse of the matrix)
# * getInverse     get the cahced value (inverse of the matrix)

makeCacheMatrix <- function(x = numeric()) {
  
  # the cached value or NULL if nothing is cached
  # since initially nothing is cached it's better set it to NULL
  cache <- NULL
  
  # store a new matrix
  setMatrix <- function(newValue) {
    x <<- newValue
    # new value assigned to matrix, flush the cache
    cache <<- NULL
  }
  
  # returns-stored matrix
  getMatrix <- function() {
    x
  }
  
  # cache the given argument 
  cacheInverse <- function(solve) {
    cache <- solve
  }
  
  # get the cached value
  getInverse <- function() {
    cache
  }
  
  # return a list. Each element is a function
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}


# This function calculates the inverse-"special"- matrix  
# makeCacheMatrix
cacheSolve <- function(y, ...) {
  # cached value
  inverse <- y$getInverse()
  # return cache value
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  # get the matrix, caclulate the inverse and store it in
  # the cache
  data <- y$getMatrix()
  inverse <- solve(data)
  y$cacheInverse(inverse)
  
  #inverse
  inverse
}
