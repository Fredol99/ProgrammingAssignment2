##############################################
########## Programming Assignment 2 ##########
##############################################

## The following functions work as the functions given in
## the assignment instructions : the syntax and the role of
## each function is quite the same

## These functions worked well on some examples given
## in the course discussion


makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        
        set <- function(y) { # Sets the matrix's value to y
                m <<- y
                inv <<- NULL
        }
        
        get <- function() m # Returns the matrix m
        
        setinv <- function(solve) inv <<- solve # Sets the 'inv' value to
        # the matrix's inverse
        
        getinv <- function() inv # Allows to get the value of 'inv'
        
        list(set = set, get = get, # Returns the CacheMatrix object
             setinv = setinv,
             getinv = getinv)

}


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- m$getinv() # Checks the current value of inv
        
        if(!is.null(inv)) { # If the inverse have already been calculated...
                message("getting cached data")
                return(inv) # ... then returns its value 
        }
        
        data <- m$get() # Else get the value of the matrix m
        inv <- solve(data, ...) # Compute its inverse...
        m$setinv(inv) # ... and store it in the CacheMatrix object
        inv
}
