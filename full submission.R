#MakeCacheMatrix#
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

#CacheSolve#
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}


#Example#
a <- diag(5,3)
a

CachedMarix <- makeCacheMatrix(a)
cacheSolve(CachedMarix)

b <- diag(2,6)
b

CachedMarix <- makeCacheMatrix(b)
cacheSolve(CachedMarix)

cacheSolve(CachedMarix)   #getting cached data

