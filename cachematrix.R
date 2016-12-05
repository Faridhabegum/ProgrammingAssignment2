## Creates a special matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) { 
    # Intialize the value of inverse matrix to NULL
    inv <- NULL 
    # set the value of matrix                      
    set <- function(y) {                    
       x <<- y                              
       inv <<- NULL                         
    }
    # get the value of matrix
    get <- function() x                     
    # set the inverse value of a given matrix  
    setinverse <- function(inverse) inv <<- inverse
    # get the inverse value of a given matrix         
    getinverse <- function() inv 
    # Passe the created functions to the working environment                          
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
                                                                                 
  }

## Computes the inverse of the special matrix returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) { 
    ## Return a matrix that is the inverse of 'x'        
    inv <- x$getinverse()
    # check if the inverse is created or not
    # Return inverted matrix from cache if it exists
    # othewise create the matrix in working environment
     if(!is.null(inv)) {
        message("getting cached data")
        # Display cached value in console
        return(inv)
     }
     # Create matrix 
     data <- x$get()
     # Set and return inverse of matrix- assume supplied matrix is invertible
     inv <- solve(data, ...)
     # Set inverted matrix in cache
     x$setinverse(inv)
     # Display matrix in console
     inv
  }
