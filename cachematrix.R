## Pair of functions that compute/cache the inverse of a matrix.
## (Or, more generally, computes/caches the result of the solve() function.)

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	s <- NULL
	set <- function(y) {
		x <<- y
		s <<- NULL
	}
	get <- function() x
	setSolve <- function(sol) s <<- sol
	getSolve <- function() s
	list(set = set, get = get,
	     setSolve = setSolve, getSolve = getSolve)

}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and
## the matrix has not changed), then cacheSolve should retrieve the inverse
## from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	s <- x$getSolve()
	if (!is.null(s)) {
		message("getting cached data")
		return(s)
	}
	data <- x$get()
	s <- solve(data, ...)
	x$setSolve(s)
	s
}
