## makeCacheMatrix would create a special matrix object
## cacheSolve calculates the inverse of the matrix
## It first checks the cache if the inverse is already determined
## If not available, then it would determine the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse_x <- NULL
    set <- function(y){
        x <<- y
        inverse_x <<- NULL
    }
    get <- function() x
    setinverse<- function(inverse) inverse_x <<- inverse
	getinverse <- function() inverse_x
	list(set = set, get = get,
	     setinverse = setinverse,
		 getinverse = getinverse)	
 	
}


## this function returns the inverse of the matrix A created by
## the function makeCacheMatrix.
## If the inverse is available in the cache, then cacheSolve retrives it and
## and if not available, then it computes it, caches it and then returns it.  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inverse_x <- x$getinverse()
		if (!is.null(inverse_x)){
		   message("getting cached inverse matrix")
           return (inverse_x)
        } else {
		  inverse_x <- solve(x$get())
		  x$setinverse(inverse_x)
		  return(inverse_x)
		}  
}
