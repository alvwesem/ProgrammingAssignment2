## Put comments here that give an overall description of what your
## functions do

## The following function returns a list of 4 nested functions that
## can be used to access these functions elsewhere. The 4 functions
## are called 'setters' and 'getters' to assign, respectively retrieve
## data of an object.

makeCacheMatrix <- function(x = matrix()) {
  inv_x<-NULL
  set<-function(y){
    x<<-y
    inv_x<<-NULL
  }
  get<-function() x
  setinv<-function(y) inv_x<<-y
  getinv<-function() inv_x
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## The following function first checks if the inverse of a matrix
## is already cached. If not, the matrix is accessed through the get
## function of makeCacheMatrix and subsequently inverted. Next,
## the inverted matrix is cashed through the function call setinv.

cacheSolve <- function(M_obj, ...) {
  inv_x<-M_obj$getinv()
  if(!is.null(inv_x)){
    message('getting cached data')
    return(inv_x)
  }
  data<-M_obj$get()
  inv_x<-solve(data,...)
  M_obj$setinv(inv_x)
  inv_x
}
