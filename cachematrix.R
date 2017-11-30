## This function creates a Matrix whose inverse is to be obtained
## This function also contains the ff. functions:
## set - set the value of the matrix
## get - get the value of the matrix
## setinverse - set the value of the inverse
## getinverse - get the value of the inverse

## NOTE: Matrix to be solved is user input

makeCacheMatrix <- function(x = matrix()) {
    mat <- NULL
    set <- function(y){
        x <<- y
        mat <<- NULL
    }
    
    get <- function() {
		x
    }
	
    setinverse <- function(inverse) {
		mat <<- inverse
	}
	
    getinverse <- function() {
		mat
    }
	
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function solves for the inverse of Matrix created above
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    mat <- x$getinverse()
    
    ## Skip the solving part if 'mat' is already exists,
    ## otherwise continue with solving for inverse
    if(!is.null(mat)){
        message("getting cached data")
        return(mat)
    }
    
    finMat <- x$get()
    mat <- solve(finMat, ...)
    x$setinverse(mat)
	
	## return the inverse
    mat
}

## This part contains sample matrix for testing

## create a sample matrix
InitMatrix <- matrix(5:8, 2, 2)

## store matrix in a temporary matrix variable
x <- makeCacheMatrix(InitMatrix)

## get the inverse of the matrix
cacheSolve(x)

## Result
    [,1] [,2]
[1,]   -4  3.5
[2,]    3 -2.5

