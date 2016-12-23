fazerCacheMatriz<-function(x = matrix()){
  
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

obterMatrizInversa <- function(x, ...){
  
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
