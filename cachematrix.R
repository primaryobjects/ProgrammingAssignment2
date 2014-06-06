## Caching the Inverse of a Matrix.
## The following set of functions allow you to invert a matrix, using cache.
## The function, makeCacheMatrix, creates a customized "matrix" object that caches its inverse.
## The function, cacheSolve, takes an object from makeCacheMatrix and returns the inverse of its matrix.
## If the inverse for the object has already been calculated, the function returns the value from cache.
##
## Example usage:
##
## Create a matrix.
## c = rbind(c(1, -1/4), c(-1/4, 1))
##
## Create a matrix cacheable object.
## m = makeCacheMatrix(c)
##
## Verify contents of matrix.
## m$get()
##
## Invert the matrix. A calculation will run the first time and return the result.
## cacheSolve(m)
##
## Invert the matrix again. This time, the cached value is returned.
## cacheSolve(m)

## Creates a customized matrix object that caches its inverse.
makeCacheMatrix <- function(x = matrix()) {
	invert <- NULL
	
	set <- function(y) {
		# Set the value of the matrix.
		x <<- y
		
		# Initialize the inverted matrix result.
		invert <<- NULL
	}
	
	get <- function() {
		# Return the value of the matrix.
		x
	}
	
	setInvert <- function(inversion) {
		# Set the inversion of the matrix.
		invert <<- inversion
	}
	
	getInvert <- function() {
		# Return the value of the inverted matrix.
		invert
	}
	
	# Return a list of the final object.
	list(set = set, get = get, setInvert = setInvert, getInvert = getInvert)
}

## Takes an object from makeCacheMatrix and returns the inverse of its matrix, either by calculating or from cache.
cacheSolve <- function(x, ...) {
	# Get the inverted value of the matrix.
	invert <- x$getInvert()
	
	if (is.null(invert)) {
		# The inversion has not yet been calculated, calculate it now.
		invert = x$get()
		# Invert the matrix.
		invert = solve(invert)
		# Store the result in the object's cache.
		x$setInvert(invert)
	}
	else {
	    print("Using cache.")
	}
	
	# Return the inverted matrix.
	invert
}
