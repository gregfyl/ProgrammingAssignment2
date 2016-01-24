## This pair of function calculate the inverse of invertible matrix.
## If the matrix inverse has been computed before, the functions retun
## cached result instead of computing again.

## makeCacheMatrix 
## 1) set the matrix 
## 2) get the matrix
## 3) set the inverse of the matrix
## 4) get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      inv_matrix <- NULL
      set <- function(y) {
            x <<- y
            inv_matrix <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) inv_matrix <<- inverse
      getinverse <- function() inv_matrix
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}     

## cacheSolve 
## first checks if the inverse has been calculated.
## If so, it gets the inverse from the cache
## Otherwise, it calculates the inverse of the matrix

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      if (det(x$get())!=0)
            ## Test if the matrix is invertible
      {
            inv_matrix <- x$getinverse()
            if(!is.null(inv_matrix)) {
                  message("getting cached matrix inverse")
                  return(inv_matrix)
            }
            data <- x$get()
            inv_matrix <- solve(data, ...)
            x$setinverse(inv_matrix)
            inv_matrix
      }
      else
            ## Return error message when the matrix is not invertible
            {
            message("non-invertible matrix.")
      }
      
}
