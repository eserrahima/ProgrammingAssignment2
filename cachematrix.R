## Following the same structure used in the vector mean example,
## the following functions calculate the inverse of a given matrix

## The following function creates a "special matrix" from a matrix given by the user
## Actually, however, the function returns a list of functions that:
## 1- Set the value of the matrix
## 2- Get the value of the matrix
## 3- Set the value of the inverse matrix
## 4- Get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv_matrix<-NULL
  set<-function(z){
    x<<-z
    inv_matrix<<-NULL
  }
  get<-function() x
  setinv<-function(solve) inv_matrix<<-solve
  getinv<-function() inv_matrix
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## The function below calculates and returns the inverse of a given "special" matrix (obtained using the makeCacheMatrix function)
## To avoid spending too much time calculating, though, it checks if the value is cached and,
## if it is, returns the cached value instead of recalculating the inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_matrix<-x$getinv()
  if(!is.null(inv_matrix)){
    message("It seems the data you need is cached, we are getting it!!")
    return(inv_matrix)
  }
  matrix<-x$get()
  inv_matrix<-solve(matrix,...)
  x$setinv(inv_matrix)
  inv_matrix
}
