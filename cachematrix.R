## This program saves a matrix to cache, in order to find its 
## inverse. We assume the matrix will always be invertible. To test it,
## we create a matrix of 3x3 with random numbers:
## test<-matrix(sample(1:20,9),3,3)
## my_inverse<-cacheSolve(makeCacheMatrix(test))
## def_inverse <- solve(test)
## when using identical(my_inverse,def_inverse), it should be true.


## This is the part that creates the matrix and defines the setters and getters.
## Saves the matrix into cache, so it can be called by cacheSolve.
## It returns a list, with names, of the functions and elements involved.

makeCacheMatrix <- function(x){
  inv <- NULL  # Creates an empty matrix where we will store the inverse matrix
  set <- function(y) {      
    x <<- y                
    inv <<- NULL           
  }
  
  get <- function() x     
  setInv <- function(solve) inv <<- solve
  getInv <- function() inv
  list(set=set, get=get, setInv=setInv, getInv=getInv) # Returns this list
  # By saving the names of each element into a list, we can use the operator $ to 
  # call it.
}


## This part uses the solve() function to find the inverse of the matrix.
## It receives a matrix to solve and returns its inverse (if it exists).

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!is.null(inv)) {   # Checks if the variable has something stored and retrieves it
    message("Getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data,...)
  x$setInv(inv)
  inv ## Returns a matrix that is the inverse of 'x'
}
