## The following pair of functions were created to cache (and possibly)the  
## inverse of a matrix.

## makeCacheMatrix creates a special "matrix" object (m) that can cache its
## inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                ##creates the var. 'm' in makeCacheMatrix env.
        set <- function(y) {
                x <<- y          ##superassigns new matrix to x
                m <<- NULL       ##because x got reassigned
        }
        get <- function() x      ## returns original matrix
        setInverse <- function(solve) m <<- solve(x) ##superassigns inverse matrix of x to m
        getInverse <- function() m  ## to obtain the stored cachevalue of the inverse x
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}
cacheSolve <- function(x, ...) {
        m <- x$getInverse()      ##calls the getInverse fct of makeCacheMatrix()
        if(!is.null(m)) {        ##if there is a cache value
                message("getting cached data") 
                return(m)
        }
        data <- x$get()          
        m <- solve(data, ...)    ## inverse matrix and store in m
        x$setInverse(m)          ## superassigns the inverse matrix to m in the makeCacheMatrix
        m
}