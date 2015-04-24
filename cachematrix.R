## The function makeCacheMatrix creates a vector (which is a list) 
## containing a function, this function: 
## sets the value of the matrix, gets the value of the matrix 
## sets the value of the invers, gets the value of the invers

makeCacheMatrix <- function(x = matrix()) { 
       m <- NULL
        set <- function(y = matrix()) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinvers <- function(solve = matrix()) m <<- solve
        getinvers <- function() m
        list(set = set, get = get,
             setinvers = setinvers,
             getinvers = getinvers)
}
 
 

## "calculates" the invers of the vector, 
## which was created in the first function 
 

cacheSolve <- function(x, ...) { 
        ## Return a matrix that is the inverse of 'x' 
        m <- x$getinvers()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinvers(m)
        m
}


