#	source("cachematrix.R")
## Put comments here that give an overall description of what your
## functions do
###	makeCacheMatrix: This function creates a special "matrix" 
###	object that can cache its inverse.
###  	cacheSolve: This function computes the inverse of the 
###	special "matrix" returned by makeCacheMatrix above. 
###	If the inverse has already been calculated (and the 
###	matrix has not changed), then cacheSolve should 
###	retrieve the inverse from the cache.
###	Computing the inverse of a square matrix can be done 
###	with the solve function in R. For example, if X is a 
###	square invertible matrix, then solve(X) returns its inverse.
###	For this assignment, assume that the matrix supplied is always invertible
#can't tell if it went to git hub
#	Uses the <<- operator to assign a value to an object in an environment 
#	that is different from the current environment. 
# 	Two functions are used to create a special object that 
#	(a) stores a numeric matrix and (b) caches its inverse.
#	The first function, makeCacheMatrix creates a special "vector", 
#	which is really a list containing a function to
#	1.	set the value of the matrix
#	2.	get the value of the matrix
#	3.	set the value of the inverse
#	4.	get the value of the inverse

makeCacheMatrix <- function(x = numeric()) {
        y=numeric()
        mi <- NULL
        set <- function(y) {
                x <<- y
                mi <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) mi <<- solve  #inverse, "solve()" performs inversion
        getinverse <- function() mi
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function checks for whether there is a cached matrix inverse (mi),
## if not it calculates and caches the inverse.

#	The following function calculates the inverse of the 
#	special "matrix" (mtrx) created with the above function. 
#	It first checks to see if the inverse has already 
#	been calculated. If so, it gets the inverse from the cache 
#	and skips the computation. Otherwise, it calculates the 
#	inverse of the matrix and sets the value of the inverse in the cache 
#	via the setinverse function.
#	Computing the inverse of a square matrix can be done with the 
#	solve function in R; if X is a square invertible matrix,
#	then solve(X) returns its inverse.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mi <- x$getinverse()
        if(!is.null(mi)) {
                message("getting cached data")
                return(mi)
        }
        data <- x$get()
        mi <- solve(data, ...)  #inverse, "solve()" performs inversion
        x$setinverse(mi)
        mi
}
#1.  myCachedMatrix <- makeCacheMatrix(matrix(c(0, -5, -4, 0), nrow=2, ncol=2, byrow=TRUE))
#2.  myCachedMatrix$get()
#3.  myCachedMatrix$getinverse()
#4.  cacheSolve(myCachedMatrix)
#5.  myCachedMatrix$getinverse()
#6.  myCachedMatrix$get() %*% myCachedMatrix$getinverse()
#7.  cacheSolve(myCachedMatrix) 
#8a. myCachedMatrix$set(matrix(c(0, -3, -2, 0), nrow=2, ncol=2, byrow=TRUE))
#8b. myCachedMatrix$get()        
#	 => you will get the new matrix
#8c. myCachedMatrix$getinverse() 
#	 => you will get NULL
#8d. cacheSolve(myCachedMatrix)
##	 => you will get another inverse matrix   
# find what is in an environment
#	ls (environment(makeCacheMatrix))	


#p = matrix(11:14,2,2  )  
#pp<-makeCacheMatrix(p)
#cacheSolve(pp)

#cacheSolve(matrix(3 * diag(3) + 1,3,3))
#	1. Pass a matrix and cache it by passing it to the makeCacheMatrix function
#	> myCachedMatrix <- makeCacheMatrix(matrix(c(0, -5, -4, 0), 
#		nrow=2, ncol=2, byrow=TRUE))
#	2. Verify that the matrix X is available in the 
#		myCachedMatrix environment but not in the global environment:
#     > myCachedMatrix$getMatrix()        
#	 => you will see a the matrix passed in 1.
#     > X                                
#	 => you will get Error object 'X' not found, or something else
#	3. Verify that the inverse of X is not yet calculated
#     > myCachedMatrix$getInverseMatrix() 
#	 => you will get NULL 
#	4.  Calculate and get its inverse with the cacheSolve function
#     >  cacheSolve(myCachedMatrix)       
#	 => you will get the inverse matrix of X
#	5. Verify the content of the cache
#     > myCachedMatrix$getInverseMatrix() 
#	 => you will get the very same inverse matrix of X
#	6. Verify that the matrix product of X and its inverse 
#		and returns the identity matrix
#     > myCachedMatrix$getMatrix() %*% myCachedMatrix$getInverseMatrix()
#	7. Invoke again cacheSolve to verify Inverse matrix is coming from
#		the cache and it is not calculated
#     > cacheSolve(myCachedMatrix)        
#	 => you will get the very same inverse matrix of X
#	8. Set a new matrix and verify the cache for the inverse matrix 
#		has been cleared.
#a     > myCachedMatrix$setMatrix(matrix(c(0, -3, -2, 0), nrow=2, ncol=2, byrow=TRUE))
#b     > myCachedMatrix$getMatrix()        
#	 => you will get the new matrix
#c     > myCachedMatrix$getInverseMatrix() 
#	 => you will get NULL
#d     > cacheSolve(myCachedMatrix)        
#	 => you will get another inverse matrix, because the matrix has changed


#8a. myCachedMatrix$set(matrix(c(0, -3, -2, 0), nrow=2, ncol=2, byrow=TRUE))
#8b. myCachedMatrix$get()        
#	 => you will get the new matrix
#8c. myCachedMatrix$getinverse() 
#	 => you will get NULL
#8d. cacheSolve(myCachedMatrix)   

#myCachedMatrix <- makeCacheMatrix(matrix(c(0, -5, -4, 0), nrow=2, ncol=2, byrow=TRUE))
#makeCacheMatrix(myCachedMatrix)

#myCachedMatrix$getMatrix()
#myCachedMatrix[get()]


#s=matrix(c(0, -5, -4, 0), nrow=2, ncol=2, byrow=TRUE)
#mcm <- function(x=matrix()){
#y=matrix()
#z<-function(y){
#x <- y
#}}
#mcm(s)
#z(s)







