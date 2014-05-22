## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inversed <- NULL
    
    #set the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    #get the matrix
    get <- function() x
    
    # set the inverse
    setinv <- function(inverse) inversed <<- inverse
    # getter the inverse
    getinv <- function() inversed
    
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        
    inverse <- x$getinv()
    
    #cache not exists
    if (is.null(inverse)) {
        #calculate the reserse
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinv(inverse)    
    }
    inverse
}
