## makeCacheMatrix is an an object containing the matrix and, once calculated, its inverse.
## The object has methods for setting and getting the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    ##  constructor - stores the input matrix in x, clears i
    i <- NULL 
    
    ## set method = change contents of variable x for matrix, clear i
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    
    # this function simply returns the matrix stored in var x 
    get <- function() x
    
    ## this stores the inverse in cache (var i)    
    setinverse <- function(inv){
        i <<- inv 
    }
    ## this returns the inverse already stored in cache    
    getinverse <- function() i 
    
    ## list the functions to make them usable    
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function returns the inverse of the matrix stored in an object of type makeCacheMatrix 
## either from cache or freshly calculated
cacheSolve <- function(x, ...) {
    ## First try from cache    
    i <- x$getinverse()
    if(!is.null(i)){
        message("found the inverse in cache")
        return(i)
    }
    ## If not found, calculate and store in cache
    mat <- x$get()
    i <- solve(mat)
    x$setinverse(i)
    i
}
