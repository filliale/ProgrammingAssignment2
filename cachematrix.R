## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function basically creates the object which holds the state.
# it has a getter and setter for the input matrix, and if the input is changed the calculated
#cached value is nulled.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
# the function creates the inverse of a matrix and uses the state model created above.
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


#inital data
sample <- matrix(runif(9, 5.0, 7.5), nrow=3,ncol=3)

#Setting up the state
calc <-  makeCacheMatrix(sample)

#inital calculation
cacheSolve(calc)

#cache hit
cacheSolve(calc)
