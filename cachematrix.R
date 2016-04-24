# The 2 functions below are used to create a special object that stores 
# a numeric matrix and caches its inverse if known, and to retrieve this inverse if known, 
# and to calculate it if not known.


## The following function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## Creates a special "matrix" object that can cache its inverse.
  ## creates a special "matric", which is really a list containing a function to
  ## 1. set the value of the matrix
  ## 2. get the value of the matrix
  ## 3. set the value of the inverse
  ## 4. get the value of the inverse
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}



## The following function returns the inverse of a special matrix created with the above function.

cacheSolve <- function(x, ...) {
  ## Returns a matrix that is the inverse of 'x'
  ## Calculates the inverse of the special "matrix" created with the above function. 
  ## However, it first checks to see if the inverse has already been calculated. 
  ## If so, it gets the inverse from the cache via the getinverse function and skips 
  ## the computation. Otherwise, it calculates the inverse of the square matrix data 
  ## and sets the value of the inverse in the cache via the setinverse function.
  
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  square_matrix_data <- x$get()
  inv <- solve(square_matrix_data, ...) 
  x$setinverse(inv)
  inv      
  
}
