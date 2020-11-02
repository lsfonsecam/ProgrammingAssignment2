## The first function caches the inverse of a given "matrix". 
## The second function returns the inverse of a "matrix"

#___
## The function caches the inverse of a given matrix, and returns that matrix and its inverse

makeCacheMatrix <- function(m = matrix()) {
  inv <<- NULL 
  matrix <- function(y){
    m <<- y
  }
  inv <<- solve(m)
  inv
  list(matrix = m, inverse = inv)
}

#   m <- matrix(rnorm(9), nrow = 3)            #  This is an example of a matrix that was used
#   makeCacheMatrix(m)

#___ 
## This second function returns the inverse of a given matrix. If this matrix was used with the "makeCacheMatrix" function 
## then the "cacheSolve" function will return the inverse previously stored in the cache, otherwise, it will return the 
## inverse of the matrix that is entered in this function

cacheSolve <- function(w = matrix()){
  if (all.equal(w,makeCacheMatrix(m)$matrix)== TRUE) {
    inv <- makeCacheMatrix(m)$inverse
    if (!is.null(inv)) {
      return(inv)
    }
  } else {
    return(solve(w))
  }
}

#     a <- matrix(rnorm(9),nrow = 3)           #  "a" is a different matrix than the "m" matrix used in the first function

#     cacheSolve(a)                            #  returns the inverse of "a" (can be compared to the one that returns "solve (a)")
#     cacheSolve(m)                            #  returns the inverse of "m" that was previously stored with the first function
