## This file contains two functions, makeCacheMatrix() and cacheSolve respectively
## 

## makeCacheMatrix will create the special object Matrix, that is actually a list containing the matrix 
## and optional caching if the inverse of the matrix has been called in the past

makeCacheMatrix <- function(x = matrix()) {
inverse<- NULL
set <- function(y){
				x <<- y
                inverse <<- NULL
				  }
		get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


##cacheSolve will check whether a cache exists for the inverse matrix and return that, if not it will solve it and store the result in cache.

cacheSolve <- function(x, ...) {
        
		inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data)
        x$setinverse(inverse)
        inverse
}
