# makeCacheMatrix() serves to create a special matrix which is really a list of a funtion to
## get the matrix value/ set inverse/ get inverse

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL;
  get <-function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(get=get,setinverse=setinverse,getinverse=getinverse)
}

##cacheSolve() can calculate the inverse of the special matrix makeCacheMatrix() created.
##When the inverse has already been calculated, it returns the existing result from the cache.
##Otherwise, it calculates the inverse and store the result into the cache, then return the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse();
  if(!is.null(m))
  {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}
