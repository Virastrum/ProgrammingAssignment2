## This consists of a pair of functions which caluclate the inverse of  
## a matrix and then stores this inverse in a cache. If the inverse of
## the matrix is requested again it uses this value rather than 
## recalculating the inverse

## This function takes in a matrix as its only argument and returns a
## list of functions which, when called, store or get information about
## the matrix.
makeCacheMatrix <- function(x = matrix()) {
    
    # When run the inverse of the matrix is unknown and set to NULL
    inverse <- NULL
    
    # This functions changes the stored matrix (x) to a new matrix (y) 
    # and sets the inverse of this new matrix to NULL. 
    
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    # This function returns the stored matrix x
    get <- function() x
    
    # This function sets the inverse of the stored matrix.
    set_inverse <- function(i) inverse <<- i
    
    # This function returns the inverse of the stored matrix.
    get_inverse <- function() inverse
    
    # This returns all the above functions as a list with the names
    # set. get, set_inverse & get_inverse.
    
    list(set=set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)

}


## This functions returns the inverse of the matrix stored in x. It
## only calculates this if it hasn't been caluclated before. Otherwise
## it returns the cached value.

cacheSolve <- function(x) {
    
    # Fetches the stored inverse value in the matrix object x
    inverse <- x$get_inverse()
    
    # If a non null value is found this value is returned.
    if( !is.null(inverse) ) {
        
        message("Returning cached inverse for this matrix")
        return(inverse)
    }
    
    #Otherwise the inverse is calculated
    else {
    
        # Fetches the stored matrix in the matrix object x
        mat <- x$get()
          
        # Calculates and sets the inverse
        inverse <- solve(mat)
        x$set_inverse(inverse)

        message("Inverse calculated and cached.")
        return(inverse)
        
    }
}
