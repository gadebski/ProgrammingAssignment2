## The function 'makeCacheMatrix' creates a special "matrix"
## which is really a list containing a function to:
##    1. set the value of the matrix
##    2. get the value of the matrix
##    3. set the value of the inverse
##    4. get the value of the inverse

makeCacheMatrix <- function(x = numeric()) {
  inverse <- NULL
  
  # set the matrix
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  # get the value of the matrix
  get <- function() x
  
  # set the inverse matrix
  setinverse <- function(inv) inverse <<- inv
  
  # get the inverse matrix
  getinverse <- function() inverse
  
  # return the list containing set and get functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The function 'cacheSolve' computes the inverse of a matrix as follows:
## first it checks if the inverse has already been computed and is stored in cache
##    - if this value is in cache
##        it retrieves it and returns thus saving computation time
##    - if the inverse value is not in cache,
##        the function computes this value
##        stores it in cache for future use
##        and returns the value

cacheSolve <- function(x, ...) {
  # check if the inverse of the matrix has been already saved in cache
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse) #from cache
  }
  # if the inverse has not been save in cache
  # retrieve the matrix value
  data <- x$get()
  #compute its inverse
  inverse <- solve(data, ...)
  
  #put the inverse into cache for future use
  x$setinverse(inverse)
  
  # return the inverted matrix
  inverse
}

## test
matr <-  matrix(1:4, 2,2) #set an invertible square matrix
matr_sp <- makeCacheMatrix(matr)
matr_sp$get() # check the value of the matrix before inverting

cacheSolve(matr_sp) #compute inverse for the first time (no value in cache)

#compute inverse for the second time - this time it is fetched from cache
cacheSolve(matr_sp)