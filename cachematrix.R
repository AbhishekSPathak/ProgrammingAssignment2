## The below two functions are created to perform inverse computation on matrices that are
## invertible. The first function computes the inverse of the assigned matrix and stores as 
## in the cache. When the inverse of the same matrix is called for computation, the second 
## function will pull the inverse value from the cache rather than computing again. 

## The first function, makeCacheMatrix, creates a special matrix that sets and gets the 
## value of the matrix, and sets and gets the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
		} 
	get <- function() x
	setinv <- function(solve) inv <<- solve
	getinv <- function() inv
	list (set = set, get = get,
		  setinv = setinv,
		  getinv = getinv) 
}

## The second function, cacheSolve, computes the inverse of the above matrix. However, if 
## the inverse is already found in the cache, that will be returned and won't be computed.
## Otherwise, this function will calculate the inverse

cacheSolve <- function(x, ...) {
		inv <- x$getinv()
		if(!is.null(inv)) {
			message ("getting cache data")
			return(inv)
		}	
		data <- x$get()
		inv <- solve(data, ...)
		x$setinv(inv)
		inv
}
