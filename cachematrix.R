## Put comments here that give an overall description of what your
## functions do

# This is essentially the example for assigment with makeVector 
# adapted to cache inverse matrix instead of vector mean.
# These functions will create an object given a matrix, and
# calculate and cache the inverted matrix.  
# Since matrix inversion is computationally expensive, we save a
# cached version, and recalculate it after it been modified with
# the setinverse() function.

## Write a short comment describing this function
# This function will cache the inverse of a matrix
# given an matrix x input into the function
# The input matrix can be set be set by either calling
# calling makeCacheMatrix(x) or mymatrix.set(x).
# The inverse matrix can be obtained with mymatrix.getinverse().
makeCacheMatrix <- function(x = matrix()) {

  i <- NULL # the inverse to be cached, initiated to NULL
  
  # function to set the the base matrix to be inverted
  set <- function(y) {  
    x <<- y            
    i <<- NULL          
  }
  
  # function to get the base matrix to be inverted
  get <- function() x   
  
  # funtion to set the inverse.  This should not be called directly
  setinverse <- function(inverse) i <<- inverse
  
  # function to get get the cached inverse matrix
  getinverse <- function() i
  
  # makeCacheMatrix returns list of function names
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function
# This function calculates the inverse of the base matrix and
# caches it IF it has not been calculated/cached.
# If it has already been calculated/cached, it just returns
# the cached inverse matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {                  # check if the inverse matrix is already cached
    message("getting cached data")   # if it is cached, return it
    return(i)
  }
  data <- x$get()                    # if it is not cached, calculate it with solve()
  i <- solve(data)
  x$setinverse(i)
  i
}
