## Functions for caching the inverse of a matrix. A special matrix object is
## defined, with the capability of caching its inverse. Plus, a function for 
## computing the inverse only if it is not cached yet is defined.

## Constructor function for a matrix object capable of caching its inverse. The
## matrix object clears cached data on matrix change, in order to ensure that
## the correct inverse matrix is used. The matrix object itself is represented
## as a list of functions (methods)

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
	setInverse  <- function (inv) inverse <<- inv
	getInverse <- function () inverse
	list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Return the inverse of a matrix. If this inverse has been computed previously,
## the cached value is used. Otherwise, the inverse is computed, cached and then
## returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inverse <- x$getInverse()
		if (!is.null(inverse)) {
			message("getting cached data")
			return(inverse)
		}
		data <- x$get()
		inverse <- solve(data)
		x$setInverse(inverse)
		inverse
}
