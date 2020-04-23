## As inversion of a matrix is usually a costly computational process, 
##      caching the inverse of a matrix rather than computing it repeatedly 
##      is beneficial. The following pair of functions are used to compute and 
##      cache the inverse of a matrix as and when required.
#
## Function "makeCacheMatrix" creates a "matrix" object that can cache its inverse.
## It creates four functions and returns a list of them to the parent environment.
makeCacheMatrix <- function(x = matrix()) { 
                           # x is set as a default empty matrix
    m <- NULL              # The result m is set to NULL
  #
  # Function#1: Setting the value of a matrix when required 
    set <- function(y) { 
      # Assigning input argument to the x object in the parent environment
        x <<- y     
      # Resetting the value of NULL to m object in the parent environment by 
      #    clearing the value of m cached by a prior execution 
        m <<- NULL        
    }
  #    
  # Function#2: Getting the value of x defined in parent environment     
    get <- function() x 
  #    
  # Function#3: Setting the inverse of the given matrix x as m
    setmatinv <- function(matinv) m <<- matinv  
  #
  # Function#4: Retrieving the value of the inverse of the matrix 
    getmatinv <- function() m  
  #
  # Creation of a list with names of all the four functions as elements and 
  #     retuning the list to the parent environment   
    list(set = set, get = get,   
         setmatinv = setmatinv,
         getmatinv = getmatinv)
}
  
## Function "cacheSolve" does the following:
## (i) checks whether the inverse of the given matrix already exists 
## (ii) if exists, it returns the value of the inverse to the parent environment 
##      with a message that it is retrieved from cache 
## (iii) if inverse of a particular matrix does not exist, it calculates, 
##       put it in cache and returns the value to the parent environment 
cacheSolve <- function(x, ...) {
  # Calling getmatinv function through the input object
    m <- x$getmatinv()   
  #    
  # Checking if the result m is NULL (for new matrix) or not (cached data)
  # Condition#1: m is not equal to NULL:
  #              There exists a cached value of inverse of the matrix
    if(!is.null(m)) {      
      # Returning the value of matrix inverse to the parent environment
      #   with a message that the value is retrieved from cache
        message("getting cached data")
        return(m)
    }
  #
  # Condition#2: m is equal to NULL (applicable for new matrix): 
  # Retrieving the Matrix x from the input object
    data <- x$get() 
  #    
  # Calculation of the inverse of the matrix
    m <- solve(data, ...) 
  #
  # Setting the calculated matrix inverse in the input object through 
  #    setmatinv function and thereby storing the value in cache
    x$setmatinv(m) 
  #
  # Returning the value of inverse of the given matrix to the parent environment
    m
}
