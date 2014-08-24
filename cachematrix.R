## Put comments here that give an overall description of what your
## functions do

#Overall objective:
#The functions below create a method to circumvent the calculation of the inverse of 
#a matrix by allocating its value and simply retrieving it when needed calculated. This essentially circumvents 
#the need to repeat a potentially time-consuming calculation. 

#"makeCacheMatrix" description:
#The primary purpose of the makeCacheMatrix function is to take in an invertible matrix (x) and attach allocation (setinverse) and recall (get and getinverse) functions in order to allow 
#subsequent functions to call the (x) matrix and different computations derived from it. In this case, the makeCacheMatrix is concerned with 
#the computation of the inverse of the (x) matrix. This makeCacheMatrix makes use the superassignement command and
#the lexical scoping that R implements to allocate values outside of its nested functions in order to pass them through other functions.


makeCacheMatrix <- function(x = matrix()) {
        y<<-x
        inverse<-NULL
        
        get<-function(){y}
        
        setinverse<-function(solve)
          {inverse<<-solve}
        
        getinverse<-function(){inverse}
        
        list(get=get,setinverse=setinverse,getinverse=getinverse)
}


# "cacheSolve" description:
# The following function uses the allocation functions in makeCacheMatrix to call any previously tagged (x) matrix
# inverse value. In the event of no inverse value is retrieved from the makeCacheMatrix function, a value is computed
# and set into allocated memory. This allocated value is retrieved in subsequent calls of the cacheSolve function and 
# prevents unnecessary repetition of the matix inverse calculation.

cacheSolve <- function(x, ...) {
  
        inverse<-x$getinverse()
        
        if(!is.null(inverse)){
          message("getting cached data")
          return(inverse)
        }
        data<-x$get()
        
        inverse<-solve(y, ...)
        
        x$setinverse(inverse)
        
        inverse
        
}
