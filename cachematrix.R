## The two functions in this text file work in tandem to create and cache 
## the inverse of the matrix passed through them and then return the cached
## inverse if it already exists.


## This function returns a list of four functions: get and set for the matrix
## passed through and get and set for the inverse of the matrix passed through.

makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function checks whether the inverse of the matrix passed through is 
## already cached. If it doesn't find it, then it solves for the inverse and 
## caches it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
