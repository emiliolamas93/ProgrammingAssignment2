## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    ##Set the value of the matrix
    s <- NULL
    set <- function(y){
    x <<- y
    s <<- NULL
    }
    ##Get the value of the matrix
    get <- function() x
    ##Get and set the value of the inverse
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set,
         get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Calculates the inverse of the matrix

cacheSolve <- function(x, ...) {
  ##Check if the inverse has been already calculated
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  ##Retreive the inverse
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  ##Return a matrix that is the inverse of 'x'
  s

}
