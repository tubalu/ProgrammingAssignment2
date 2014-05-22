makeCacheMatrix<-function(x = matrix()) {
    inversed <- NULL
    
    #set the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    #get the matrix
    get <- function() x
    
    # set the inverse
    setInverseMatrix <- function(inverse) inversed <<- inverse
    # getter the inverse
    getInverseMatrix <- function() inversed
    
    
    list(set = set, get = get, setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)
}

cacheSolve <- function(x, ...) {

    inverse <- x$getInverseMatrix()
    
    #cache not exists it will calculated from solve function, otherwise, it will be return directly.
    if (is.null(inverse)) {
        #calculate the reserse
        data <- x$get()
        inverse <- solve(data, ...)
        x$setInverseMatrix(inverse)    
    }
   
    inverse
}
