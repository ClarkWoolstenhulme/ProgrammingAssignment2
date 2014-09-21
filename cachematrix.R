## Put comments here that give an overall description of what your
## functions do

## This function allows you to create a "cacheing matrix",
## which will store a copy of its own inversion for quick access

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        ## Getters and setters for the core matrix 
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x

        ## Getters and setters for the inversion 
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        
        ## Finally, this maker function returns a list of getters and setters
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function executes solve() on a "cacheing matrix"
## which will either return the cached version of the inversion, or calculate and cache the inversion

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        ## Use the handy getter function we created
        s <- x$getInverse()
        if(!is.null(s)) {
        ## Hooray, it's cached, this will be so fast
                message("getting cached data")
                return(s)
        }
        
        ## Boo, it wasn't cached (or we would have returned already) so let us get the matrix...
        data <- x$get()

        ## ...solve for the inverse...
        s <- solve(data)
        
        ## ...cache that inverse for future use...
        x$setInverse(s)
        
        ## ...and return that inverse for immediate use.
        s
}