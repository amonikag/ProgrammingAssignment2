#for cache matrix 
makeCacheMatrix <- function(x = matrix()){
      inv <- NULL #initialized inverse function
      set <- function(y){ #setting inverse function
            x <<- y 
            inv <<- NULL 
      }
      get <- function() {x}
      setInverse <- function(inverse) {inv <<- inverse} 
      getInverse <- function() {inv} #taking inverse matrix
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) #making the list of inverse matrix
}

#for cache matrix.
cacheSolve <- function(x, ...){
      inv <- x$getInverse() #taking the inverse matrix
      if(!is.null(inv)){ #condition to check the whether the matrix is not null
            message("getting cached data")
            return(inv)
      }
      mat <- x$get()
      inv <- solve(mat, ...) 
      x$setInverse(inv) 
      inv
}
