## There will be two functions that are used to cache and solve the inverse of a matrix


## makeCacheMatrix caches a matrix that is the inverse of another if it has not 
## been set before


makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        setCachedInverse <- function(solve) m<<- solve
        getInverse <- function() m
        list(set=set,get=get,setCachedInverse=setCachedInverse,getInverse=getInverse)
}


## cacheSolve will return the cached inverse if one exists.  Otherwise it will calculate
## the inverse and then cache it.

cacheSolve <- function(x, ...){
        ## Return a matrix that is the inverse of 'x'
        
        m<- x$getInverse()
        if (!is.null(m)) {
                message("getting the cached inverse")
                return(m)
        }
        
        data <- x$get()
        message("Calculating the inverse instead of retrieving the cache")
        m <- solve(data, ...)
        x$setCachedInverse(m)
        m
}