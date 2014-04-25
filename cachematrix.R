## This is a pair of functions that cache the inverse of a matrix


## This function creates a special "matrix" object that can cache 
## its inverse.

makeCacheMatrix <- function(x = matrix()) {

    
    theI <- NULL   ## theI is the variabe for inverse matrix
    set <- function(y) {    ## set new original matrix
        x <<- y
        theI <<- NULL
    }
    get <- function() x     ## get the cached the original matrix
    setinverse <- function(caculatedinverse) theI <<-caculatedinverse ## cache the invere matrix
    getinverse <- function() theI  ## get cached the invere matrix
    list(set = set, get = get,  ## create special matrix list for original matrix
         setinverse = setinverse,
         getinverse = getinverse)
    
    
    
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache
## x is the the speicial "matrix" list
## x0 is the original "matrix". This is needed to check if matrix changes

cacheSolve <- function(x, x0) {
    data <- x$get()
    if(all(length(x0)==length(data)) && all(data==x0)){  # check if matrix changes
        
        theI <- x$getinverse()
        if(!is.null(theI)) {
            message("getting cached data")
            
            return(theI)  # no change, just get cached inverse matrix 
        }
        else{   # initialise new matrix list
            message("New matrix. Caculate inverse matrix")
            data <- x$get()
            theI <- solve(data)
            x$setinverse(theI)
            theI   ## return inverse matrix
        }
    }
    else{      # Matrix changed. update new matrix list and cache new inverse matrix 
        message("Matrix changed. Caculate inverse matrix")
        
        
        x$set(x0)    ## update matrix 
        data <- x$get()
        theI <- solve(data)
        x$setinverse(theI)
        theI             ## return new inverse matrix
    }
}
