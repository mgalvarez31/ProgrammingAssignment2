## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {  # the matrix x is initialized as a function argument

                    m <- NULL  # The matrix m is set to NULL, initializing it as an object within 
                               # the makeCacheMatrix environment. m store the matrix inverted
                    
                    set <- function(y) {
                      x <<- y    # Assign the input argument to the x object in the parent environment
                      m <<- NULL # Assign the value of NULL to the m object in the parent environment. 
                                 # Clears any value of m that had been cached by a prior execution of cacheSolve().
                    }
                    get <- function() x  # Defines the getter for the vector x
                    setinverse <- function(solve) m <<- solve  #makeCacheMatrix() defines the setter for the inverse m. 
                                                               #Assign the input argument to the value of m in the parent environment.
                    
                    getinverse <- function() m  # Defines the getter for the inverse m
                    list(set = set, get = get,  # Assigns each of these functions as an element within a list(), and returns it to the parent environment.
                         setinverse = setinverse, # Each element in the list is named
                         getinverse = getinverse)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix functionsource

cacheSolve <- function(x, ...) {       
              m <- x$getinverse() # the function attempts to retrieve a inverse from the object passed in as the argument. 
                                # it calls the getinverse() function on the input object.
              
              #If the inverse has already been calculated (and the matrix has not changed):
              #if the value here is not equal to NULL, we have a valid, 
              #cached inverse and can return it to the parent environment
              if(!is.null(m)) {   
                message("getting cached data")
                return(m)  # then CacheSolve should retrieve the inverse from the cache
              }
              
              # If the result of !is.null(m) is FALSE
              
              data <- x$get()  # cachesolve() gets the matrix from the input object
              m <- solve(data, ...)  # calculate the inverse of the matrix input
              x$setinverse(m) # it use the setinverse() function on the input object to set the inverse in the input object
              m   # returns the value of the inverse to the parent environment
  
}
