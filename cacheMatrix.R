# Coursera "R Programming" 013
# Homework Assignment #2

makeCacheMatrix <- function(x = matrix()){
  # This function creates a matrix (or takes one)
  # and creates a list data structure used to cache
  # the inverse of the matrix. It returns a list,
  # with the matrix itself and it's (possibly NULL)
  # inverse, along with two functions to access/set the inverse.
  
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  # This function takes as an input a cacheable
  # matrix (data structure created using makeCacheMatrix)
  # and returns its inverse, using the cache if
  # the inverse was computed in the past.
  
  inv <- x$getinverse()
  
  # If inverse is already computed, return it from cache.
  if (!is.null(inv)) {
    message("Getting Cached Data")
    return(inv)
  }
  
  # Compute inverse, cache it, return it.
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}