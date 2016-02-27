## This functions cache the inverse of a matrix and gets the inverse
## if it hasn't been calculated

## Creates a list for the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setsolve<-function(solve) m<<-solve
  getsolve<-function() m
  list(set=set,get=get,setsolve=setsolve,getsolve=getsolve)

}


## Checks if the inverse of the matrix exists in the cache. 
## If it does, it returns it's value. Else, it calculates the inverse
## and stores it in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getsolve()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data)
  x$setsolve(m)
  m
}
