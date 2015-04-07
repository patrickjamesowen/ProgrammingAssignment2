## Coursera Data Science Specialisation - R-Programming - Assignment 2

#This is a collection of two functions to to cache a matrix and its inverse and invert a matrix if it hasn't
#already been inverted and cached

#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  #initialising the inverse matrix as  null
  inverse <- NULL
  
  #Set the value of the matrix
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  #Getting the value of the matrix
  get <- function() x
  
  #setting the inverse of the matrix
  set_inverse <- function(inverse_input) inverse <<- inverse_input
  
  #getting the inverse of the matrix
  get_inverse <- function() inverse
  
  #making a list of all the results - last line so this is what gets returned
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  #this bit checks if we already have a cached inverse and gets it if we do
  inverse <- x$get_inverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }  
  
  #there's an implicit else here - if we don't already have the invese of x we now get x...
  data <- x$get()
  
  #...and calculate its inverse...
  inverse <- solve(data, ...)
  
  #...and then caches it...
  x$set_inverse(inverse)
  
  #...and finally returns it..
  inverse
  
}
