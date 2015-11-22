##This program is responsible for caching the inverse of a matrix and getting the value
##from the cached object for every call until the matrix is reset again.

##Instructions to run this program
#1) Source the program in Rstudio
#2) create the matrix during the call of makeCacheMatrix()
#a <- makeCacheMatrix( matrix(c(1,7,11,13), nrow = 2, ncol = 2) );
#3) summary(a);
#4) a$getMatrix();- Gets the stored matrix value in a
#5) cacheSolve(a)- Now calculates inverse
#6) Again cacheSolve(a)- Now returns the value from cached object with a message
#'Getting values from cached data'


# makeCacheMatrix is a function that returns a list of functions
# Its purpose is to store a martix and a cached value of the inverse of the 
# matrix. Contains the following functions:
# * setMatrix      set the value of a matrix
# * getMatrix      get the value of a matrix
# * cacheInverse   get the cahced value (inverse of the matrix)
# * getInverse     get the cahced value (inverse of the matrix)
#
makeCacheMatrix <- function(x = matrix()) {
  
  # holds the cached value or NULL if nothing is cached
  # initially nothing is cached so set it to NULL
  cache <- NULL
  
  # store a matrix
  setMatrix <- function(newValue) {
    x <<- newValue
    # since the matrix is assigned a new value, flush the cache
    cache <<- NULL
  }
  
  # returns the stored matrix
  getMatrix <- function() {
    x
  }
  
  # cache the given argument 
  cacheInverse <- function(solve) {
    cache <<- solve
  }
  
  # get the cached value
  getInverse <- function() {
    cache
  }
  
  # return a list. Each named element of the list is a function
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}


# The following function calculates the inverse of a "special" matrix created with 
# makeCacheMatrix
cacheSolve <- function(y, ...) {
  # get the cached value
  inverse <- y$getInverse()
  # if a cached value exists return it
  if(!is.null(inverse)) {
    message("Getting values from cached data")
    return(inverse)
  }
  # otherwise get the matrix, caclulate the inverse and store it in
  # the cache
  data <- y$getMatrix()
  inverse <- solve(data)
  y$cacheInverse(inverse)
  
  # return the inverse
  inverse
}
