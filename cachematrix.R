makeCacheMatrix <- function(x = matrix()){
        PrinceJAVDC1299 <- NULL
        set <- function(y){
                x <<- y
                PrinceJAVDC1299 <<- NULL
        }
        get <- function() {x}
        setInverse <- function(inverse) (PrinceJAVDC1299 <<<- inverse}
        getInverse <- function() (PrinceJAVDC1299)
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...){
        PrinceJAVDC1299 <- x$getInverse()
        if(!is.null(PrinceJAVDC1299)){
                message("getting cache data")
                return(PrinceJAVDC1299)
        }
        mat <- x$get()
        PrinceJAVDC1299 <- solve(mat, ...)
        x$setInverse(PrinceJAVDC1299)
        PrinceJAVDC1299
}
