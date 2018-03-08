## Put comments here that give an overall description of what your
## functions do

## Collects matrix to be examined
makeCacheMatrix <- function(x = matrix()) 
  {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
    }
  get <- function() x
  set_inv_matrix <- function(inv) m <<- inv
  get_inv_matrix <- function() m
  list(set = set, get = get, set_inv_matrix = set_inv_matrix, get_inv_matrix = get_inv_matrix)
  }	

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    
    m<-x$get_inv_matrix()
    
    ## Return a matrix that is the inverse of 'x' if already cached
    if(!is.null(m)) {
     message("getting cached data")
      return(m)
    }
  
    ## Return a matrix that is the inverse of 'x' if not already cached
      data1 <- x$get()
    m <- solve(data1, ...)
    x$set_inv_matrix(m)
     m
     
  }
  
  
