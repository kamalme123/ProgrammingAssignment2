## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# set the value of the matrix
# get the value of the matrix
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL #set this up as a NULL variable now so that you can check later to see if anything has been stored in it
  set <- function(y) { # setup the value of the matrix
    x <<- y
    i <<- NULL
  } 
  get <- function() x #store the function into get
  setmatrix <- function(inverse) i <<- inverse #invert the matrix and store it in 'i'
  getmatrix <- function() i #get the matrix back out
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix) # create a list of the variables involved
}



## Write a short comment describing this function

# set the value of the inverse
# get the value of the inverse
cacheSolve <- function(x, ...) {
  i <- x$getmatrix()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get() #retrieve the original matrix
  i <- solve(data, ...) #create an inverse version of the original matrix so that you can compare to the cached version
  x$setmatrix(i)
  i
}

