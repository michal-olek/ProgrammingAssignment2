## wrapping creation of  matrix so one can create an "ecosystem" for functions dealing with cache

makeCacheMatrix <- function(x = matrix()) {
	m<-NULL
	set <- function(y )
		 {
			x <<- y
			m <<- NULL
		 }
	get <- function() x
	setSolve <- function(solve) m <<- solve
	getSolve <- function() m
	list (set = set, get= get, setSolve = setSolve, getSolve = getSolve)
}


## if there exists cached value return it - if no, calculate it and store for 
## later use in an "ecosystem" we created in previous method

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getSolve()
	if(!is.null(m)) {
		message("getting cached data")
		return (m)
	}
	data <-x$get()
	m <- solve(data, ...)
	x$setSolve(m)
	m
}
