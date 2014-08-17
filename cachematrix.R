## Assignment: Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse, 
## which is really a matrix containing a function to: 
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the inverse
## 4) get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    
    ## Return a cached matrix that is the inverse of 'x'
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  
  ## Return a matrix that is the inverse of 'x'
  inv
}

## Unit Test
## my_matrix <- matrix(c(2, 2, 3, 2), ncol=2, nrow=2)
## cm <- makeCacheMatrix(my_matrix)
## cacheSolve(cm)
## cacheSolve(cm)
