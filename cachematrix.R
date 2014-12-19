
## makeCacheMatrix is a function that takes a matrix as a input and returns a list with set, get, setinverse, # getinverse which acts as a setter and getter for the matrix and the inverse.

makeCacheMatrix <- function(x = matrix()) {
	 i <- NULL
	#sets the value of the matrix and assigns NULL to inverse
         set <- function(y) { 
                 x <<- y
                 i <<- NULL
         }
	# get function retrieves the matrix
         get <- function()
	    {
		 x
	    }
	# setinverse function set the value of the inverse variable
         setinverse <- function(inverse) { i <<- inverse }
  	#getinverse function retrieves the value of inverse variable       
	getinverse <- function() { i }
	
	## makeCacheMatrix returns list
         list(set = set, get = get,
             setinverse = setinverse,
              getinverse = getinverse)

}


## cacheSolve is a function that takes a list as an input and finds the cached inverse if exists else calculates
# the inverse using solve() function in R and caches it for future use. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	# fetch the value of inverse
  	i <- x$getinverse() 
	# If a not NULL value exists, retrieve the cached value and return it. In that case the the cacheSolve 	# function will not be executed further.
         if(!is.null(i)) {
                 message("getting cached data")
                 return(i)
         }
	# If inverse is NULL, fetch the matrix
         data <- x$get()
	# calculate the inverse of the matrix using solve function
         i <- solve(data, ...)
	# cache the inverse
         x$setinverse(i)
         i

}
