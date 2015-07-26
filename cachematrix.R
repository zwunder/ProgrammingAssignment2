##The following two functions will be used to cut down on computation times
##for taking the inveses of non-singular matrices. Taking the inverse of a 
##matrix can be costly operation, so it can be beneficial to store an 
##often-used value rather than computing it repeatedly.

## makeCacheMatrix stores matrix information into a separate environment.

makeCacheMatrix <- function(x = matrix()) {
      m<-NULL
      set <- function(y){
          x <<- y
          m <<- NULL
      }
      get <- function() x
      setinv <- function(y) m <<- solve(y)
      getinv <- function() m
      return(list(set=set,get=get,setinv=setinv,getinv=getinv))
}


##Input the name of the environment containing your matrix and it's inverse.
##If the inverse is saved in the environment it extracts the object and returns
##it. Otherwise it calculates the inverse and returns it anyway. If it was
##cached, a message will say so.

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)){
          message("Data Cached")
          return(m)
        }   else {
              data <- x$get()
              m <- solve(data)
              x$setinv(data)
              return(m)
        }
}
