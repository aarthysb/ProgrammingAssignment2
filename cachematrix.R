#Objective:
#Calculating inverse of a matrix is a costly operation to be performed everytime.
#So the following funtions facilitates caching the result of the inverse of a matrix,
#so that the cached result can be accessed for subsequent calls instead of
#calculating it every time.


#This function stores the matrix and returns a list with following functions:
# get - returns the input matrix
# set - sets the matrix and resets the inverse
# getInverse - returns the inverse of the input matrix
# setInverse - sets the inverse of the input matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  #sets the matrix and resets the inverse to null
  set <- function(m){
    x <<- m
    inverse <<- NULL
  }
  #returns the base matrix 
  get <- function(){
    x
  }
  #assigns the inverse of the matrix into 'inverse' variable
  setInverse <- function(inv){
    inverse <<- inv
  }
  #returns the cached inverse(will be null if inverse is not cached)
  getInverse <- function(){
    inverse
  }
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

#cacheSolve method gets the cached value of the inverse and returns it, if it is
#not null.
#Solves the inverse and caches it otherwise.


cacheSolve <- function(x, ...) {
  #Check if cached value if not null and return it
  inverse <- x$getInverse()
  if(!is.null(inverse)){
    return(inverse)
  }
  #Calculate inverse of the matrix and set(cache) it
  inverse <- solve(a=x$get())
  x$setInverse(inverse)
  inverse
}
