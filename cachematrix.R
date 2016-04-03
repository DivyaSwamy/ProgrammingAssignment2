
## The functions makeCacheMatrix & cacheSolve were written for programming assignment 2 Coursera. 
## Divya Swaminathan

## makeCachematrix:  Input matrix is first passed through makeCacheMatrix. It creates a special matrix, a list 
## containing a function to set the value of the matrix, get the value of the  matrix, sets the inverse value of the matrix using 
## function solve, gets the inverse value of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  m<- NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<- function() x
  setinverse<- function(solve) m<<-solve
  getinverse<- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
  
}

## cacheSolve: Calculates the inverse of a matrix (provided the matrix inverse exists).
## Before calculating the inverse it checks if the inverse has already been calculated, if so it
## gets the inverse from the cache skipping the computation.  imput for cache solve is the output of function 
## makeCacheMatrix. 

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if (!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<- x$get()
  m<-solve(data, ...)
  x$setinverse(m)
  m
  
  ## Return a matrix that is the inverse of 'x'
}
