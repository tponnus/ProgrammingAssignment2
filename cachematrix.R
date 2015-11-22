makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

assgnmt1<-makeCacheMatrix(matrix(1:4,2,2))##assigning value 
assgnmt1$get()##to view matrix format
assgnmt1$getinverse()

cacheSolve<- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv      
}
cacheSolve(assgnmt1)
cacheSolve(assgnmt1)