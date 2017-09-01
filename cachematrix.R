## There are two functions in this file.  
## makeCacheMatrix is used to cache a matrix
## cacheSolve is used to compute the inverse of the matrix if it is not already cached.

## makeCacheMatrix is used to cache a matrix. 
## Input is a matrix
## Output is a special matrix which is cached.

makeCacheMatrix <- function(x = matrix()) {
  #create place holder to store the inverse matrix
  cached_inverse_matrix <- NULL
  
  #construction function to save the input matrix
  set <- function(y) {
    x <<- y
    cached_inverse_matrix <<- NULL
  }
  
  #construction function to get the input matrix
  get <- function() x
  
  #construction function to save the inverse matrix
  setInverseMatrix <- function(inverse_matrix) cached_inverse_matrix <<- inverse_matrix
  
  #construction function to get the inverse matrix from cache
  getInverseMatrix <- function() cached_inverse_matrix
  
  #Returns the special matrix objects
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
  

}


## cacheSolve is used to compute the inverse of a matrix, it is was already cached it will fetch it from the cache.
##input is a specal matrix which is resturned by the makeCacheMatrix function
##output is a inverse matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  #get the cached inverse matrix
  m <- x$getInverseMatrix()
  
  #if present return the cached matrix
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #if not present get the input matrix
  data <- x$get()
  
  #compute inverse
  m <- solve(data, ...)
  
  #cache the inverse matrix
  x$setInverseMatrix(m)
  
  #return the inverse matrix
  m
  
}
