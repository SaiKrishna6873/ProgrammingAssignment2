## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The argument of the below function "makeCacheMatrix" includes the all the elements of the matrix 
## along with its dimensions. After the argument is passed into the function, get() funtion is used 
## to print the matrix. Whenever getInverse() is called for the first time, it shows NULL as it's 
## not calculated yet.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse  <- function(Inverse) inv <<- Inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

## When the variable in which the above funtion after giving the arguments is assigned is passed as 
## an argument in the function "cacheSolve", the inverse of the given matrix is calculated using 
## solve() and shown as output (for the 1st time). If the same is called repeatedly, then the inverse 
## stored in the cache is returned along with a message "getting cached data".

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}

