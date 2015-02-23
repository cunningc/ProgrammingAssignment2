## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Create function "makeCacheMatrix and coerce passed arg to be matrix 
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL ## initialize m as null
        set <- function(y) { ## create "set" function for re-setting cache vectors
                x <<- y ## assign x in calling env
                m <<- NULL ## assign m in calling env
        }
        get <- function() x  ## make "get" function to return x 
        setmatrix <- function(matrix) m <<- matrix ## create "setmatrix" to assign cache
        getmatrix <- function() m  ## create "getmatrix" to return cached value
        ## return a list that has callable functions defined above
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix() ## use "getmatrix" function from x to retrieve cached value
        if(!is.null(m)) { ## if not null show message and return cached value
                message("getting cached data")
                return(m)
        }
        ## if cached value is null then do the following
        data <- x$get() ## get pointer to caching funciton from x
        m <- solve(data, ...) ## assign inverse matrix to m
        x$setmatrix(m)  ## set cache with updated data
        m
}
