## With the following two functions, a special matrix-object is created with the feature to cache the inverse of a matrix.
## To get the inverse, solve() is used and it is assumed, that the matrix is invertable.

# This function creates a special matrix object that can cache its inverse.
# The parameter x is the initial value of the special matrix-object. Default is an empty matrix.
# The object has the functions:
#  set(y) and get() to set and get the values of the object.
#  setinverse(y) and getinverse() to set and get the inverse of the object.
# The function returns a list containing the four functions.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# The following function calculates the inverse of the special matrix created with the above function. 
# However, it first checks to see if the inverse has already been calculated. 
#  If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
