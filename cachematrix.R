## This code is the second programming assignment for the Coursera course, R Programming, 
## provided by John Hopkins University.
## The following two functions cache the inverse of a matrix. 

## makeCacheMatrix() creates a special "matrix" object that can cache its inverse. 
## will return a list of functions, set, get, setinverse, getinverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
    

}


## cachesolve() calculates the inverse of the special "matrix" created with the above function. 
## It first checks to see if the inverse has already been calculated. If it has, it gets the inverse from the cache and skips the computation. 
## Else, it calculates the inverse and sets the value of the inverse in the cache using the setinverse function.
## This function assumes that the matrix suppplied is always invvertibe. 


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

