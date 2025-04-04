## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        
        #here m is temporary inverse of matrix 
        #this local function is used to get input matrix and initialize a empty m
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        #define a get() function for retrieving the matrix x
        #define a setinv() function to store the newly computed inverse of matrix
        #define getinv() for retrieving the inverse of matrix x
        
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        
        #retrieve the stored inverse of matrix
        m <- x$getinv()
        
        #check weather the inverse of matrix if empty, if not empty, then no need to compute,
        #just return the cache inverse of matrix
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        
        #get the matrix using get() function
        #compute the new inverse of matrix using solve()
        data <- x$get()
        m <- solve(data, ...)
        
        #use setinv() to store newly computed inverse of matrix, and return it.
        x$setinv(m)
        m
}









