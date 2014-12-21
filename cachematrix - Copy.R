# Functions 'makeCacheMatrix' and 'cacheSolve' utilize properties of the '<<-' operator
# to store an inverse of a special "matrix" in the cache.
# 'makeCacheMatrix' creates a special "matrix" and stores it and its inverse in the cache.
# 'cacheSolve' checks if there is a correct inverse stored in the cache.
# If there isn't, it computes and deposits a new one in the cache.


## 'makeCacheMatrix' function creates a special "matrix" object that can cache its inverse.
## The inverse is computed and stored in the cache.

makeCacheMatrix <- function(x = matrix()) {
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(newinverse) {
    inverse <<- newinverse
  }
  
  inverse <<- solve(x)
  
  getinverse <- function() {inverse}
   
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## 'cacheSolve' function computes the inverse of the special
## "matrix" returned by 'makeCacheMatrix' above. 
## The function checks if (1) the inverse matrix in the cache is null, 
## (2) if the inverse is the same (if matrix has change or the inverse is incorrect).
## Then the function gives a corresponding message and calculated the inverse if necessary.

cacheSolve <- function(x, ...) { 
  
  y <- solve(x)
  
  if(is.null(inverse)) {
    
    message("Inverse is null. Calculating new inverse.")
    message("New inverse:")
    inverse <<- solve(x)
  
  } else {
      
      if((sum(all.equal(y,inverse)==TRUE))==1) {
        
        message("Inverse is correct!")
        message("Inverse:")
        
      } else {
        
        message("The matrix has changed or the inverse is incorrect.")  
        message("New and correct inverse is")
        inverse <<- y        
        
      }   
  } 
  
  print(inverse)
  
}
