## The two functions below are used to find the inverse of a matrix x. 
## If the inverse has already been found, it is returned without the need to
## recalculate the inverse, thus, saves processing time.
## NOTE: makeCacheMatrix(x) has to be called prior to cacheSolve(x).

## --------------------------------------------------------------------------------
## makeCacheMatrix Comments
## This function creates a special R object that 
## 1. Initializes a variable local_matrix
## 2. Provides function get() to obtain "raw" matrix (of which one needs to find 
##    its inverse)
## 3. Provides function setImatrix() getImatrix()
## --------------------------------------------------------------------------------

makeCacheMatrix <- function(x=matrix()) {
  local_matrix <- NULL
  get <- function() x
  setImatrix <- function(Imatrix) local_matrix <<- Imatrix
  getImatrix <- function() local_matrix
  
  # return a list of functions as an R object
  list(get=get, setImatrix=setImatrix, getImatrix=getImatrix)
}

## --------------------------------------------------------------------------------
## This function does the actual inversing of matrix x.  It first checks if the
## inverse matrix is in the cache; if yes, returns the result, otherwise the 
## inverse of x is calculated, saved to cached, and returns the inverse matrix.
## --------------------------------------------------------------------------------

cacheSolve <- function(x) {
  local_matrix <- x$getImatrix()
  if(!is.null(local_matrix)){
    message("Cached data found. Getting result from cache.")
    return(local_matrix)
  }
  else {
    message("No cached data found. Calculating inverse matrix")
    data <- x$get() 
    local_matrix <- solve(data) 
    x$setImatrix(local_matrix) 
    message("Done.")
    return(local_matrix)
  }
}