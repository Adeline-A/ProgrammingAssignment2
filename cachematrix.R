## The following functions allow to cache the inverse of an invertible matrix 
## by storing it in a special 'matrix' (actually a list of functions), thus not
## needing to repeat the costly computation every time the inverse is needed.


## The following function creates a special 'matrix' object which is actually 
## a list of functions allowing to set the value of the matrix, get the value 
## of the matrix, set the value of the inverse and get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
    ## Function #1: sets the value of the matrix using the matrix passed as
    ## argument to the main function
  
  get <- function() x
    ## Function #2: gets the value of the matrix
  
  setInverse <- function(inverse) inv <<- inverse
    ## Function #3: sets the value of matrix inverse using the inverse passed
    ## as argument. The actual inverse calculation is not made here but rather
    ## in the cacheSolve function below.
  
  getInverse <- function() inv
    ## Function #4: gets the value of matrix inverse
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
    ## Return a list of functions to set/get the matrix and set/get its inverse
}



## The following function calculates the inverse of a special 'matrix' created 
## using the function makeCacheMatrix. It first checks wether or not the inverse 
## has already been calculated and stored in the object.If yes, it skips the 
## calculation and displays the cached inverse. If no, it calculates the inverse 
## and stores it the cache using the setInverse function.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
    ## Checks to see wether the matrix inverse is already in the cache
  
  if(!is.null(inv)){
    message("Getting cached inverse")
    return(inv)
  }
    ## If there is already a cached inverse, the function returns it and exits.
  
  data <- x$get()
  inv <- solve(data,...)
    ## There is no cached inverse: the function gets the matrix and computes its inverse
  
  x$setInverse(inv)
    ## Stores the computed inverse in the matrix cache
  
  inv
    ## Return a matrix that is the inverse of 'x'
}
