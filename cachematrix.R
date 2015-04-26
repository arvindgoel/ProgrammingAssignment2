##
## Matrix inversion is usually a costly computation and there is
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly. Functions in this file cache the inverse of a matrix
## 
## Credits: http://adv-r.had.co.nz/Functions.html
## 
## steps for unit testing are documented in "UnitTestingResults.txt"


##
## This function creates a special "matrix" object 
## and caches its inverse. It is assumed
## that the matrix supplied is always invertible.
##
makeCacheMatrix <- function(x = matrix()) {

	r <- NULL
	set <- function(y) {
		x <<- y
		y <<- NULL 
	}
	get <- function() { x }
	setinverse <- function(solve) { r <<- solve }
	getinverse <- function() { r }
	list(set = set, get = get,
		setinverse = setinverse,
                getinverse = getinverse)
}


##
## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` retrieves the inverse from the cache.
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	
	r <- x$getinverse()
	if(!is.null(r)) {
		message("getting cached data")
		return (r)
	}
	data <- x$get()
	r <- solve(data)
	x$setinverse(r)
	r


}

