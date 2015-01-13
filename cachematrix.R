## this script defines two functions used to cache the inverse of a matrices
## makeCacheMatrix(cur_matrix) generates a list of functions to read and write a matrix and its inverse
## cacheSolve(cacheMatrix) returns the inverse of such a cacheMatrix, using a cached result if the inverse has been calculated earlier

## makeCacheMatrix(cur_matrix): returns a matrix object in the form of a list of functions [get(), set(new_matrix), set_inverse(new_inverse), get_inverse()]
## "cur_matrix" is current value of the matrix (initially given by the parameter cur_matrix), and the variable "inverse" is used to store the inverse of cur_matrix (or NULL if the inverse has not been calculated yet)
## the get() and get_inverse() commands are used to read the value of the matrix and its inverse
## the set(new_matrix) and set_inverse(new_inverse) commands are used to set the value of the matrix and its inverse
makeCacheMatrix <- function(cur_matrix = matrix()) {
  inverse <- NULL
  set <- function(new_matrix) {
    cur_matrix <<- new_matrix
    inverse <<- NULL
  }
  get <- function() cur_matrix
  set_inverse <- function(new_inverse) inverse <<- new_inverse
  get_inverse <- function() inverse
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

## cacheSolve(cacheMatrix): returns the inverse of cacheMatrix, where cacheMatrix is a list as given by makeCacheMatrix
## if the inverse of cacheMatrix has been calculated earlier, the earlier result is re-used
cacheSolve <- function(cacheMatrix) {
  inverse <- cacheMatrix$get_inverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  inverse <- solve( cacheMatrix$get() )
  cacheMatrix$set_inverse(inverse)
  inverse
}