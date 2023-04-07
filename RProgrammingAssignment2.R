## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ##This function creates a special "matrix" object that can cache its inverse
  inver<-NULL
  ##inver would hold value of matrix inverse
  set<- function(y){
    x<<-y
    ##value of matrix
    inver<<- NULL
  }
  get<-function() x
  ##defines the get function-returns value of matrix argument
  setinverse<-function(inverse) inver<<-inverse
  ##assign values of of inver in parent environment
  getinverse<-function()inver
  ##gets the value of inver when called
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inver<- x$getinverse()
  if(!is.null(inver)){
    message("getting cached dATA")
    return(inver)
  }
  data<- x$get()
  inv<- solve(data, ...)
  x$setinverse(inver)
  inver
}