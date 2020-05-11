## makeCacheMatrix and cacheSolve work together.
## makeCacheMatrix creates a 'special' matrix which allows caching computations.
## cacheSolve conceptually inverses the 'special' matrix returned by makeCacheMatrix.
##
## Example:
## 
##     x <- makeCacheMatrix(matrix(rnorm(9), 3, 3))
##     cacheSolve(x)  ## Here inversed matrix is calculated, cached in 'x' and returned.
##     cacheSolve(x)  ## Here x remembers that inversion was already calculated and cached
##                    ## and returns the cached value.


## Creates a 'special' matrix allowing caching computations. Actually a list of functions is returned.
## x - matrix data

makeCacheMatrix <- function(x = matrix()) {
	cache <- NULL
	
	set <- function(y) {
		x <<- y
		cache <<- NULL
	}
	
	get <- function() x
	
	setResult <- function(z) cache <<- z
	
	getResult <- function() cache
	
	
	list(
		set = set,
		get = get,
		setres = setResult,
		getres = getResult
	)
}


## Inverse matrix x. It returns cached value if the inverse was already computed and cached
## or computes the inverse, caches it and returns it.

cacheSolve <- function(x, ...) {
	cachedInversed <- x$getres()
	if ( !is.null(cachedInversed) ) {
		message("getting cached data")
	} else {
		message("calculating data")
		cachedInversed <- solve(x$get())
		x$setres(cachedInversed)
	}
	cachedInversed
}
