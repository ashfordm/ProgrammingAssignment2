##create an inverse matrix and cache it
##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  ##set the value of the matrix
  set<-function(y){
  	##<<- operator which can be used to assign a value to an object in an environment that is different from the current environment.
	x<<-y
  	m<<-NULL
	}
	##get the value of the matrix
	get<-function() x
	##set the value of the inverse matrix
	setmatrix<-function(solve) m<<- solve
	##get the value of the inverse matrix
	getmatrix<-function() m
	##create the list
	list(set=set, get=get,
   		setmatrix=setmatrix,
   		getmatrix=getmatrix)
}

##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
cacheSolve <- function(x=matrix(), ...) {
	##Load the inverse matrix
    	m<-x$getmatrix()
	##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
	##Run it twice to test
    	if(!is.null(m)){
      		message("getting cached data")
      		return(m)
    	}
	##Load matrix
    	matrix <- x$get() 
	##Computing the inverse of a square matrix can be done with the solve function in R. For example, 
	##if X is a square invertible matrix, then solve(X) returns its inverse.
	##Generate Inverse matrix when it does not exit
    	m<-solve(matrix, ...)
    	##Add it to the list
	x$setmatrix(m)
	##Return the inverse matrix
    	m
}

##test case
##build matrix and cache
##a<-makeCacheMatrix()
##a$set(matrix(1:4,2,2))
##a$get()
##a$getmatrix()
##create inversse matix
##cacheSolve(a)
##a$getmatrix()
##test if it is all ready loaded
##cacheSolve(a)
##a$getmatrix()
