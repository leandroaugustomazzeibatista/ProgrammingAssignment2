
# Some variables  were written in Portuguese,but the functions were writen in english

# the makeCacheMatriz function verifies if the inverse of a matrix has already been calculated and returns this inverse matriz

makeCacheMatrix<-function(x = matrix()){
  
  matrizInversa <-NULL
  
  set<-function(y){
    
    x<<-y
    
    matrizInversa<<-NULL
    
  }
  
  get <- function() x
  
  setInversao <- function(inversao) matrizInversa <<- inversao
  
  getInversao<-function() matrizInversa
  
  list(set = set,get = get,
       setInversao=setInversao,
       getInversao=getInversao)
  
}

    #This function calculates the inverse of a matriz for the first time

  cacheSolve <- function(x, ...){
  
  matrizInversa<-x$getInversao()
  
  if(!is.null(matrizInversa)){
    
    message("Recuperando a Matriz Inversa da Memória Cache")
    
    return(matrizInversa)
  }
  
  matriz <- x$get()
  
  matrizInversa <- solve(matriz, ...)
  
  x$setInversao(matrizInversa)
  
  matrizInversa
}
