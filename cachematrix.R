#Matrix inversion is usually a costly computation and there may be some 
#benefit to caching the inverse of a matrix rather than compute it repeatedly

#Function makeCacheMatrix returns a list containing a set of functions
#The main task of the function is to create a special object that stores a matrix and its inverse

#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inve <- NULL
    set <- function(y) {                #<-this part can be used to change content of matrix
        x <<- y
        inve <<- NULL
    }
    get <- function() x                 #<-This part is about showing the content of matrix
    setInverse <- function(inverse) inve <<- inverse        #<-changing cached inverse
    getInverse <- function() inve       #<-here you can get your matrixs inverse
    list (set = set, get = get, 
          setInverse = setInverse, getInverse = getInverse)         #<-this part creates a list conatining
}                                                                   #all functions used on the matrix x


# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
#above. If the inverse has already been calculated 
#(and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inve <- x$getInverse()                  #<-this part uses a function which comes from a list made in
    if (!is.null(inve)) {                   #makeCacheMatrix function
        message('getting cached data')      #<-checking if inverse is empty, returns cached, inversed matrix
        return(inve)                        #and a message. if there is no inverse, if does nothing
    }
    data <- x$get()                         #<-getting data from list, made in makeCacheMatrix
    inve <- solve(data, ...)                #<-solving equation and creating inverse 
    x$setInverse(inve)                      #<-using function from list, made in makeCacheMatrix
    inve                                    #<-returns inversed matrix
}

#Testing functions:
test_matrix <- makeCacheMatrix(matrix(1:4, 2, 2, byrow= TRUE))
test_matrix$get() 
test_matrix$getInverse() #there is no inverse yet
cacheSolve(test_matrix) #returns inversed matrix
cacheSolve(test_matrix) #returns inversed matrix and message, that there is cached data
test_matrix$getInverse() #this element of the list is no longer NULL
#changing content
test_matrix$set(matrix(1:4, 2, 2, byrow=FALSE))
test_matrix$get() #new matrix is on
test_matrix$getInverse() #again NULL
cacheSolve(test_matrix)
cacheSolve(test_matrix)
test_matrix$getInverse() #saved inverse