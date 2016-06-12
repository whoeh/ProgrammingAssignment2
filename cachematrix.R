## makeMatrix function assigns a matrix and contains solve function. With x2$setmatrix(solve(x1)) sets the inverse matrix

makeMatrix <- function(x = matrix()) 
{
    m<-NULL
    set<-function(y)
    {
        x<<-y
        m<<-NULL
    }
    get<-function() x
    setmatrix<-function(solve) m<<- solve
    getmatrix<-function() m
    list(set=set, get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix)
}

## cacheSolve function first checks if solving function previously runned  and checks if  cache  has the output
## (checks m if it is not null)
## if m is not null then returns already cached value and prints "getting cached data"
## if it is null then runs the makeMatrix function

cacheSolve <- function(x, ...) {
    m<-x$getmatrix()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    data<-x$get()
    m<-solve(data, ...)
    x$setmatrix(m)
    m
}



## use case of makeMatrix function and cacheSolve function 
## first creates matrix then assign z1 to z2 via makeMatrix function
## then solves / inverses z1 and sets the output 
## with getmatrix show the solved output , with cashSolve also shows output from the cache

x1 <- matrix(1:4,2,2)
x2 <- makeMatrix(x1)
## first run (calculates)
cacheSolve(x2)
## first run (returns from cache)
cacheSolve(x2)