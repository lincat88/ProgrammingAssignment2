## Together these functions set and store a matrix and its inverse 

## makeCacheMatrix is a list of functions that stores and reads a matrix and its inverse
makeCacheMatrix<-function(x=matrix()) {
        m<-NULL
        set<-function(y) { ##modifies an existing object
                x<<-y ##these are global so that they work in the get/setmean/getmean funcs below
                m<<-null
        }
        get<-function() x ##function returning the original matrix
        setinverse<-function(solve) m<<-solve ##function for setting the inverse of the matrix
        getinverse<-function() m ##function for returning the inverse of the matrix
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) ##this is a list of functions
}



##cacheSolve checks to see if the inverse was already calculated and stored, if not, it does those things

cacheSolve<-function(x,...) {
        ##return a matrix that is the inverse of x
        m<- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data<-x$get()
        m<-solve(data,...)
        x$setinverse(m)
        m
}

##code to test whether my code is working properly, expect the negative signs to switch cols
matrix1<-matrix(c(-1,-2,1,1),2,2)
x<-makeCacheMatrix(matrix1)
x$get()
cacheSolve(x)


