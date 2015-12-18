## Two functions and their applications are shown here
## 1. makeCacheMatrix: function creates a special "matrix" object that can cache its inverse
## 2. cacheSolve: function computes the inverse of the special "matrix" returned by the above makeCacheMatrix 


## The makeCacheMatrix function creates an object 
## which  sets and gets a matrix and its inverse 
## (scoping rules are used)

makeCacheMatrix  <- function(x = matrix()) {
  Inv <- NULL
  set <- function(y) {
    x <<- y
    Inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) Inv <<- inverse
  getInverse <- function() Inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The cacheSolve function computes the inverse of a matrix (if not already cached) 
## and cache it to the above makeCacheMatrix function; 
## otherwise the message "Getting cached data' is displayed 

cacheSolve  <- function(x,...) {
  Inv <- x$getInverse()
  if(!is.null(Inv)) {
    message("Getting cached data")
  }
  data <- x$get()
  Inv <- solve(data,...)
  x$setInverse(Inv)
}

## Applications: 

z <- matrix(sample(1:10,4*4,replace =T),nrow=4) # A matrix 4X4 is created 
m_obj<-makeCacheMatrix(z) # A matrix object is created (m_obj)

m_obj$get() # Get the matrix
m_obj$getInverse()  # Get its inverse (here NULL, not cached)

cacheSolve(m_obj) # The inverse of the matrix is calculated and is cached
m_obj$getInverse()  # Get the inverse of the matrix (here not NULL, cached)

cacheSolve(m_obj) # The inverse of the matrix is cached ; Message 'Getting cached data' is displayed

m_obj$set(z)  # Reset, the cached inverse of the matrix is deleted 
m_obj$getInverse() # Get the inverse of the matrix (Here NULL, not cached)

cacheSolve(m_obj) # The inverse of the matrix is computed and cached again (No message 'Getting cached')

round(m_obj$get()%*%m_obj$getInverse())  # Product of a matrix with its inverse gives the matrix identity



