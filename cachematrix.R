## Matrix inversion consumes lots of resources and performing that very frequently 
## can be very costly. To mitigate this, inverse of matrix can be cached.
## Following functions can be used for that purpose

##  makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  
  get <- function() x
  
  setInvmatrix <- function(invMatrix) inv <<- invMatrix
  
  getInvmatrix <- function() inv
  
  list(set=set, get=get, setInvmatrix=setInvmatrix, getInvmatrix=getInvmatrix)
}


## Following fuction calcuates the inverse of a matrix. It first checks to see if the 
## inverse has already been calculated. If so, it gets inverse from cache and skips remaining
## computation. Otherwise, it calculates the inverse and sets the value in cache via setInvmatrix function

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInvmatrix()
  
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  matrix <- x$get()
  
  inv <- solve(matrix, ...)
  
  x$setInvmatrix(inv)
  inv
}
