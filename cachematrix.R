## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x=matrix()) {
  local_matrix <- NULL
  get <- function() x
  setImatrix <- function(Imatrix) local_matrix <<- Imatrix
  getImatrix <- function() local_matrix
  
  # return a list of functions as an R object
  list(get=get, setImatrix=setImatrix, getImatrix=getImatrix)
}

## Write a short comment describing this function

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