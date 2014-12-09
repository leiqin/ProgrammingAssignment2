## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#
# creates a special "matrix", which is really a list containing a function to
#
#    set the value of the matrix
#    get the value of the matrix
#    set the value of the inversion
#    get the value of the inversion
# 

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setInversion <- function(inversion) i <<- inversion
	getInversion <- function() i
	list(set=set, get=get,
		 setInversion=setInversion,
		 getInversion=getInversion)
}


## Write a short comment describing this function
#
# calculates the inversion of the special "matrix" created with 
# the makeCacheMatrix function. However, it first checks to see 
# if the inversion has already been calculated. If so, 
# it gets the inversion from the cache and skips the computation. 
# Otherwise, it calculates the inversion of the data and 
# sets the value of the inversion in the cache via 
# the setInversion function.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	i <- x$getInversion()
	if (!is.null(i)) {
		message('getting cached data')
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setInversion(i)
	i
}
