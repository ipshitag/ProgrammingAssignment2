## A set of functions to compute and cache the inverse of a square matrix

## A function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	##initialize i as NULL
	i<-NULL
	##create set function to set values of x and i
	set <- function(y) {
		x<<-y
		i<<-NULL
		}
	##create get function which returns x matrix	
	get <- function() x
	##create setinv value to cache inverse of matrix
	setinv <-function(solve) i <<- solve
	##create getinv function to return cached inverse
	getinv <- function() i
	##create list of functions
	list(set = set, get = get, 
		setinv = setinv, 
		getinv = getinv)
}

## A function that takes the special "matrix" created with makeCacheMatrix as argument
## if inverse has been calculated, returns cached inverse
## if not, it calculates the inverse and saves to cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of special matrix 'x'
		## this matrix must be created with the makeCacheMatrix function
		
		##check value of i to see if inverse is cached
		i<- x$getinv()
		##if inverse matrix is cached i is not NULL
		if(!is.null(i)){
			##return message that data is cached
			message("Getting cached data")
			##return cached inverse
			return(i)
		}
		##if inverse matrix is NOT cached, i IS NULL
		##load values of matrix x as variable matrix1
		matrix1 <- x$get()
		##make value of i equal to inverse of matrix1 with solve function
		i <- solve(matrix1)
		##set inverse value i for special matrix 'x'
		x$setinv(i)
		##return inverse of the matrix
		i	
}
