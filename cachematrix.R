## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


# makeCacheMatrix Function
# This function creates matrix that can cache its inverse.

 #Set and Get values to the Matrix
  #Set and Get Inverse values to the Matrix

makeCacheMatrix <-function(x = matrix()) {
        i <-NULL
      set <-function(y) {
               x <<-y
               i <<-NULL
        }
        get <-function() x
        setinverse<-function(inverse) i <<-inverse
        getinverse<-function() i
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
# cacheSolve Function 
# This function computes the inverse of the matrix returned by 
# makeCacheMatrix function. If the inverse has already been 
# calculated (and the matrix has not changed), then the cachesolve 
# should retrieve the inverse from the cache.




cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
          message("getting cached data")
          return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
