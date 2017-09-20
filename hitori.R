# Capítulo 3. 
# UN EJEMPLO PARA APRENDER R. EL HITORI

# FUNCIÓN QUE DIBUJA EL TABLERO

dibujatablero<-function(hitori,tachada){
  
}

# FUNCIÓN PRINCIPAL

#Creo la función que se llama juega.hitori
juega.hitori<-function(){
  
  #Obtenemos de un fichero los números del hitori
  #read.table nos devuelve un data frame
  hitori<-read.table("hitori.txt")
  
  #Lo pasamos a matriz
  hitori<-as.matrix(hitori)
  
  #Creamos una matriz que representa las casillas tachadas llena de ceros
  tachada<-matriz(0, nrow=6, ncol=6)
  
  #Dibujamos el tablero con las dos matrices con la función de dibujar
  dibujatablero(hitori, tachada)
  
  #Inicializo el juego 
  juega=T
  
  #Empezamos a tachar casillas hasta resolver el juego
  while(juega){
    #Sacamos un menú y guardamos la opción en jugamos
    jugamos<-menu(c("Tachar casilla", "Recuperar casilla"))
    
    #Sacamos por pantalla texto
    cat("Elija fila y columna \n")
    #Recogemos el resultado en la variable pos
    pos<-scan(n=2)
    
    #Comprobamos la posición comparando los vectores (componente a componente)
    #Nos debería dar cero la suma de valores lógicos en la comparación
    if(sum(pos<c(1,1) | pos>c(6,6))>0){
      #Escribimos mensaje de error por pantalla
      cat("La posición no es válida \n")
    }
    else{
      # Caso jugamos=1: TACHAR CASILLA
      if(jugamos==1){
        
      }
      # Caso jugamos=2: RECUPERAR CASILLA
      
    }
    
    
  }
}

