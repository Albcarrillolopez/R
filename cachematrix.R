## In this two functions, I calculate the inverse of a matrix 
## and I cache it.

## First, I build this function, in order to cache the inverse of
## the given matrix in the future.

makeCacheMatrix <- function(x = matrix()) {
       inverse<-NULL                       
       set<-function(matrix){
             x<<-matrix                  
             inverse<<-NULL              
       }
       get<-function() x
       setInverse<- function(solve) inverse<<- solve
       getInverse<-function() inverse
  list(set=set,get=get,setInverse=setInverse,
       getInverse=getInverse)  
}


## Then, I build a second function that checks if the inverse of x
##  has been already calculated:
##       YES: It returns the inverse
##       NO: It calculate the inverse (with solve) and caches it.

cacheSolve <- function(x, ...) {
       inverse<- x$getInverse()
       if(!is.null(inverse)){
             return(inverse)
       }
       aux<- x$get()
       inverse<- solve(aux,...)
       x$setInverse(inverse)
       inverse
}

