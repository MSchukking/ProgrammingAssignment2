## Together, the functions makeCacheMatrix.R and cacheSolve.R cache the inverse 
## of a matrix

## The makeCacheMatrix.R function creates a special "matrix" object that can 
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        ## solve(= s) is set to NULL as a placeholder for a future value
        
        s <- NULL
        
        ## Defining the set() function. 
        ## set() is included so that once the object of makeCacheMatrix() is 
        ## created, its value can be changed without resetting its value. This 
        ## is not used the first time the object of makeCacheMatrix() is used, 
        ## but will be used when the object is used the second, third, ... time
        
        set <- function(y) {
                x <<- y     
                s <<- NULL   
        }
        
        ## Defining the getter for the vector x
        ## Since x is not defined within get(), R takes advantage of lexical scoping
        ## and retrieves x from the parent environment of makeVector(). 
        ## This means it returns the vector x
        
        get <- function() x
        
        ## Setting the solve (s) to solve 
        
        setsolve <- function(Solve) s <<- Solve
        
        ## Returns the solve (s)
        
        getsolve <- function() s
        
        ## Returning a new object in a list that contains all of the functions 
        ## just defined. This part of the code names each element in the list: 
        ## Each element in the list is created with an elementName: 
        ## This allows us to use the $ to access the functions by name 
        
        list(set = set, ## gives name "set" to the set() function 
             get = get, ## gives name "get" to the get() function 
             setsolve = setsolve, ## gives name "setsolve" to the setsolve() function  
             getsolve = getsolve) ## gives name "getsolve" to the getsolve() function 
        
}


## The cacheSolve.R function computes the inverse of the special "matrix" 
## object returned by makeCacheMatrix.R. If the inverse has already been 
## calculated(and the matrix has not changed), then this function should 
## retrieve the inverse from the cache with the message "getting cached data". 

cacheSolve <-function(x, ...) { 
        
        s <- x$getsolve()
        
        ## Checks if the inverse matrix has already been calculated. If so, it 
        ## skips the computation and it retrieves the inverse matrix from the 
        ## cache and throws message "getting cached data": 
        
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        
        ## If the inverse matrix has not been calculated yet, it calculates 
        ## the inverse of the data and sets the inverted matrix in the cache 
        ## via the setsolve function
        
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
