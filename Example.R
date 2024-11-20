setwd("E://GitHub_coursera")
a <- diag(5,3)
a

source("makeCacheMatrix.R")
source("cacheSolve.R")
CachedMarix <- makeCacheMatrix(a)
cacheSolve(CachedMarix)

b <- diag(2,6)
b

CachedMarix <- makeCacheMatrix(b)
cacheSolve(CachedMarix)

cacheSolve(CachedMarix)   #getting cached data
