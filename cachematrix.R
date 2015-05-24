## Program Level Comment: The following functions work together 
## to create a square invertible matrix
## and make the inverse of the matrix available in the cache environment
##
## Function: makeCacheMatrix creates a special matrix object
## that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  ## x is a square invertible matrix
  # initialize to NULL
  inv1 <-NULL
  # create the matrix in the working environment
  set<-function(y){
    x<<-y
    inv1 <<-NULL
  }
  #get the value of the matrix
  get<-function() x
  # invert the matrix and store in cache
  setinverse <-function(inv2) inv1<<-inv2
  # get the value of the the inverted matrix from cache
  getinverse <-function() inv1
  # return the created list of functions to the working environment
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}
##
## cacheSolve calcluates the inverse of the matrix created in makeCacheMatrix
## If the inverted matrix does not exist in cache,
## it it created in the working environment and it's inverted value
## is stored in cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x' from cache
  inv1<-x$getinverse()
  # return inverted matrix from cache if it exists
  # else create the matrix in working environment
  if (!is.null(inv1)) {
    message("getting cached data")
    
    # display matrix in console
    return(inv1)
  }
  # create matrix since it does not exist
  matrix <- x$get()
  #invert the matrix using solve function
  inv1<-solve(matrix,...)
  # set the inverse and return inverse matrix
  x$setinverse(inv1)
  return(inv1)
}

