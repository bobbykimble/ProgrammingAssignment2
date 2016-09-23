## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix takes a square matrix as input and returns a list used for input in cacheSolve.
## the list contains functions that do the following:
## set matrix
## get matrix
## set inverse
## get inverse

makeCacheMatrix <- function(x = matrix()) {      
        inv = NULL
        set = function(y) {
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function
## cacheSolve calculates the inverse of x. the function will check to see if the inverse has already been calculated and will skip the computation if so.

cacheSolve <- function(x, ...) {
		inv = x$getinv()
        
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        x$setinv(inv)
        
        return(inv)
}
