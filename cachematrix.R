## This function creates a Matrix whose inverse is to be obtained

makeCacheMatrix <- function(x = matrix()) {
    mat <- NULL
    set <- function(y){
        x <<- y
        mat <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(inverse) mat <<- inverse
    getinverse <- function() mat
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function gets the inverse of Matrix 'x' created above

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    mat <- x$getinverse()
    
    ## Skip the solving part if 'mat' is already the inverse matrix,
    ## otherwise continue with solving for inverse
    if(!is.null(mat)){
        message("getting cached data")
        return(mat)
    }
    
    finMat <- x$get()
    mat <- solve(finMat, ...)
    x$setinverse(mat)
    mat
}
