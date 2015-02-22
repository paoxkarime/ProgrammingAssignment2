## Programming Assignment 2: Caching the Inverse of a Matrix 


## Creates a special matrix object that stores a matrix and caches its inverse

makeCacheMatrix <- function(x = matrix()) {  

m <- NULL                    ## this variable will store the inverse 
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setInverse <- function(solve) m <<- solve(x)   ## stores the data 
      getInverse <- function() m     ## returns the data (inverse)
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)

}


## This function returns the inverse of the special matrix created above. It retrieves the inverse from the cache if the inverse has already been calculated before. 

cacheSolve <- function(x, ...) {
        
        
        m <- x$getInverse()         ## first looks for the inverse 
      if(!is.null(m)) {
            message("getting cached data")        
            return(m)             ##if its found, return the inverse from the cache 
      }
      data <- x$get()
      m <- solve(data, ...)       ## solves the matrix if not found
      x$setInverse(m)
      m                            ## returns the inverse 
}
