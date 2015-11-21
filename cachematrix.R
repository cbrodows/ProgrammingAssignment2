## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# NOTE: I understand that one does not need to provide
#       a return statement in R and that functions can be
#       one line.  However, I prefer to be consistent with
#       bracket notation and to provide a return statement
#       to be consistent with most other C-based languages.
makeCacheMatrix <- function(x = matrix()) {
  inv_mat = NULL
  
  # Setter - Pushes input argument into cache and 
  #          forces reinitialization + caching of 
  #          of inverse member matrix
  setMatrix <- function(in_matrix) {
    x <<- in_matrix
    inv_mat <<- NULL
  }
  
  # Getter - Returns cached original matrix
  getMatrix <- function() {
    return (x)
  }
  
  # Setter - Applies solve function to member matrix.  In essence,
  #          this applies a type of overloaded solve method.
  #
  # In practice, we'd either want to have exception catching 
  # here to prevent non-invertible matrices from entering routine,
  # or we would have a conditional block in here to handle the
  # potential use case.
  setInverse <- function(solve) {
    inv_mat <<- solve
  }
  
  # Getter - Returns inverted matrix
  getInverse <- function() {
    return (inv_mat)
  }
  
  # Create a list of functions that may be referred to and
  # called by the cacheSolver.
  #
  # TODO: This would be much better suited as one or more 
  #       classes; need to eventually see how R does OOD
  return (list(set = setMatrix, get = getMatrix, 
               setInv = setInverse, getInv = getInverse))
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return ax matrix that is the inverse of 'x'

  # apply the getter to see what is in cache  
  inv_matrix <- x$getInv()
  
  # If there is no inverted matrix in the cache, apply our
  # "overloaded" solve method to generated the inverted matrix.
  # Afterwards, store the computed value in cache and return the
  # value locally.
  if (is.null(inv_matrix)) {
    message("No inverted matrix in cache; calculating inverse")
    inv_matrix <- solve(x$get(), ...)
    x$setInv(inv_matrix)
  }
  
  # Nothing to do if already inverted except log a message.  It
  # would be nice to save the contents of an input matrix and
  # to recalculate anytime that original matrix changes, and 
  # that wouldn't be difficult to do in the above method...
  else {
    message("Matrix has already been inverted; returning value from cache...")  
  }
  return (inv_matrix)
}
