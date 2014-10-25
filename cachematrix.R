## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	matrix <- NULL
	set <- function(y) {
		x <<- y
		matrix <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) matrix <<- solve
	getinverse <- function() matrix
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)

}


## This function computes the inverse of the  special "matrix" returned by
##   makeCacheMatrix above.
## If theinverse is already calculated, it retrieves the inverse from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	matrix <- x$getinverse()
	if(!is.null(matrix)) {
		message("getting cached data")
		return(matrix)
	}
	data <- x$get()
	matrix <- solve(data, ...)
	x$setinverse(matrix)
	matrix
}
