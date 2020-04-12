## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  ##Fonction to set a new matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ##Fonction to return the matrix
  get <- function() x
  ##Fonction to inverse the matrix
  setinverse <- function(inverse) m <<- inverse
  ##Fonction to return the matrix inverse
  getinverse <- function() m
  
  ##Creation of the output with the different results
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


##This function computes the inverse of the special "matrix" returned 
##by makeCacheMatrix above
cacheSolve <- function(x, ...) {
  
  m <- x$getinverse()
  ##if the matrix has already been inversed
  if(!is.null(m)) { 
    message("getting cached data.")
    return(m)
  }
  
  data <- x$get()
  ##Inversion of the matrix
  m <- solve(data)
  x$setinverse(m)
  
  ##Return the matrix
  m
}
