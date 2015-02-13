
#### This is an assigment 2 of R-Programming course on coursera
#### Right by skydome20

##################################################################################################
#                                                                                                # 
#   Matrix inversion is usually a costly computation                                             #
#   and there may be some benefit to caching the inverse of a matrix                             #
#   rather than compute it repeatedly (there are also alternatives to matrix inversion           #
#   that we will not discuss here).                                                              #
#                                                                                                # 
##################################################################################################

#Your assignment is to write a pair of functions that cache the inverse of a matrix.(matrix')

##################################################################################################
#                                                                                                #
#  (Hint)                                                                                        #
#  Computing the inverse of a square matrix can be done with the solve function in R.            #
#  For example, if X is a square invertible matrix, then solve(X) returns its inverse.           #  
#                                                                                                #
##################################################################################################


##################################################################################################
#                                                                                                #
#  This function creates a special "matrix" object that can cache its inverse.                   #
#                                                                                                #
##################################################################################################
makeCacheMatrix <- function(x = matrix()) {
    
    cache <- NULL
    # 1. set the value of the matrix
    set <- function(y) {
        x <<- y
        cache <<- NULL
    }
    
    # 2. get the value from the matrix
    get <- function() x
    
    # 3. set the value of matrix' to cache
    setmatrix <- function(inverse_matrix) cache <<- inverse_matrix
    
    # 4. get the value  matrix' from cache
    getmatrix <- function() cache
    
    #  which is really a list containing a function to 
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)

}

##################################################################################################
#                                                                                                #
#  This function computes the inverse of the special "matrix"                                    #
#  returned by makeCacheMatrix above.                                                            #
#  If the inverse has already been calculated (and the matrix has not changed),                  #
#  then the cachesolve should retrieve the inverse from the cache.                               #
#                                                                                                #
##################################################################################################
cacheSolve <- function(x, ...) {
        
    # get value of mean from cache
    inverse_matrix <- x$getmean()
    
    # it first checks to see if the mean has already been calculated.(= in the cache) 
    # If so, it gets the mean from the cache and skips the computation. 
    if(!is.null(inverse_matrix)) {
        message("getting cached inverse of matrix")
        return(inverse_matrix)
    }
    
    # Otherwise,
    # it calculates the inverst of the matrix(matrix') 
    # and sets the value of the matrix' to cache via the "setmatrix" function.
    matrix <- x$get()                        # get value from original matrix
    inverse_matrix <- solve(matrix, ...)     # calculate the inverse of matrix
    x$setmatrix(inverse_matrix)              # set matrix' to the cache
    
    # Return a matrix that is the inverse of matrix
    inverse_matrix

}
