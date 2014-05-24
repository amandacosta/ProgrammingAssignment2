##Comments that give an overall description of what your function do:
##This function caches the inverse of a matrix for faster computation
##caching the inverse is better than computing the inversion repeatedly
##solve() is a great function for this assignment once you know what it does.        
##You need somewhat intermediate programming skills to do this assignment 
#ask a programmer friend to help you and Read 'An introduction to R' or/and 
##'R cookbook', they have great examples and Chamber's Statistics and Computing.
##If you have basic programming skills,
##you need at least 20h to complete this assignment

##Write a short comment describing this function[sic]:      
##first you have to create a special "matrix" object that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
        ##set matrix object value
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## get matrix value
        get <- function() x
        
        ##set inversion of matrix
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        
        ##get inversion of matrix 
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

##Computing the inverse of the Matrix above. If the inverse has not already 
##been calculated it will retrieve it from the cache. 

cacheSolve<- function(x, ...) {
        ##get the inversion
        m <- x$getinverse()
        
        ##checking the cache is being returned
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ##if so, get inverse
        m <- solve(x$get())
        x$setinverse(m)
        m
}
