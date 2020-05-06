## These functions calculate the inverse of a matrix. If it's the first time such matrix
##appears, it will calculate it. Else it will recover it from cache

##This function has the constructors to set and get the inverse of the matrix, that is to 
##store it the first time it's calculated or to retrieve it if it exists already

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## this function tries to get the inverse from the cache. If it's not found, it will calculate it
##and then store it in the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}