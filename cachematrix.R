
## The following creates a special matrix object that can cache its 
## inverse.

makeCacheMatrix <- function(m = matrix()) {
        i <- NULL
        # set the value of the matrix
        set <- function(a) {
                m <<- a
                i <<- NULL
        }
        # get the value of the matrix
        get <- function() m
        
        # set the value of inverse of the matrix
        setinv <- function(inv) i <<- inv
        
        # get the value of inverse of the matrix
        getinv <- function() i
        
        # create the list of functions objects
        list(   set=set, 
                get=get, 
                setinv=setinv, 
                getinv=getinv)
}


## The following function computes the inverse of the special matrix 
## returned by the makeCacheMatrix function above. If the inverse has 
## already been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
                # retrieves the inverse from the cache
                i <- x$getinv() 
                if(!is.null(i)) {
                        # the inverse exists in cache
                        message("the cacheSolve function is now getting cached data.")
                        # return the cached inverse and exit function
                        return(i)
        }
        # the inverse is not in cache so it has to be calculated
        elem <- x$get()
        
        # here we suppose that the matrix in elem variable
        # is always invertible and use the R solve function 
        # that returns the inverse of the square invertible 
        # matrix given as an argument.
        message("the cacheSolve function is now calculating the inverse matrix, please wait...")
        i <- solve(elem)
        # set the calculated inverse in cache
        x$setinv(i)    
        # returns the calculated inverse
        i
}
