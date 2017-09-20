# Capítulo 3. 
# UN EJEMPLO PARA APRENDER R. EL HITORI

# FUNCIÓN QUE DIBUJA EL TABLERO

dibujatablero<-function(hitori,tachada){
  #Las casillas tachadas se marcan con una cruz
  
  plot(1,1, col=0, xlim=c(0,6), ylim=c(0,6), axes=F, xlab="", ylab="")
  
  for(i in 0:6){
    lines(c(0,6), c(i,i), lwd=2)
    lines(c(i,i), c(0,6), lwd=2)
  }
  
  for(i in 1:6){
    for(j in 1:6){
      text(j-0.5, 7-i-0.5, hitor[i,j], col=1, cex=2)
      if(tachada[i,j]==1){
        text(j-0.5, 7-i-0.5, "X", cex=5, col=2)
      }
    }
  }
  
}


# FUNCIÓN PRINCIPAL

#Creo la función que se llama juega.hitori
juega.hitori<-function(){
  
  #Obtenemos de un fichero los números del hitori
  #v1.read.table nos devuelve un data frame
  
  #hitori<-read.table("/Users/Maria/Documents/Pruebas/hitori.rtf")
  
  #v2.Leo en un vector
  #hitori<-scan("/Users/Maria/Documents/Pruebas/hitori.rtf", sep="")
  
  #Lo pasamos a matriz
  #hitori<-as.matrix(hitori)
  
  #Creo la matriz del hitori de prueba 6x6
  num<-c(2,2,4,3,4,1,6,5,5,1,1,3,3,5,6,1,4,3,5,1,1,1,6,2,3,4,3,2,5,1,4,3,6,6,2,5)
  
  hitori<-as.matrix(num, nrow= 6,ncol=6)
  #Creamos una matriz que representa las casillas tachadas llena de ceros
  tachada<-matrix(0, nrow=6, ncol=6)
  
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
    #Recogemos el resultado en la variable pos (que es un vector)
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
        
        #Miramos si la casilla está tachada: recorremos la matriz si tiene 1 para tachada
        if(tachada(pos[1], pos[2])==1){
          cat("La posición elegida ya estaba tachada\n")
        }
        else 
        {
          #Comprobamos si se puede tachar la casilla (es decir, que no tenga casillas tachadas ortogonales)
          
          #Determinamos las filas a mirar para los lados del tablero
          if (pos[1]==1){
            mira.fila=2
          }
          else if(pos[6]==1){
            mira.fila=5
          }
          else{
            #Creo un vector con las filas de arriba y abajo
            miro.fila=c(pos[1]-1, pos[1]+1)
          }
          
          #Determinamos las columnas a mirar
          if(pos[2]==1){
            mira.columna=2
          }
          else if(pos[2]==6){
            mira.columna=5
          }
          else{
            #Creo un vector con las columnas izq y dcha
            miro.columna=c(pos[2]-1, pos[2]+1)
          }
          
          #Comprobamos que las casillas no estén tachadas
          #Con el sum hacemos la comprobación de suma con valores lógicos
          
          if(sum(tachada[pos[1],mira.columna])>0|sum(tachada[mira.fila, pos[2]])>0){
            cat("No se puede tachar la casilla seleccionada\n")
          }
          
          else{
            tachada[pos[1], pos[2]]=1
          }
        }
        
        
      }
      # Caso jugamos=2: RECUPERAR CASILLA
      else if(jugamos==2){
        if(tachada[pos[1], pos[2]]==0){
          cat("La posición elegida no está tachada")
        }
        else{
          tachada[pos[1], pos[2]]=0
        }
      }
      #dibujamos otra vez el tablero con el cambio realizado en cada jugada
      dibujatablero(hitori, tachada)
      
      #Vamos mirando si el juego se ha acabado o no
      #Creo un vector numérico vacio para filas y columnas
      
      fila<-numeric()
      col<-numeric()
      
      #Recorremos la matriz
      for(i in 1:6) {
        fila[i]=sum(table(hitori[i, tachada[i, ]==0])>1)
        col[i]=sum(table(hitori[tachada[,i]==0,i])>1)
      }
      
      if(sum(fila,col)>0){
        cat("Se repiten números\n")
        
      }
      else{
        cat("ENHORABUENA\n")
        
        #Cambiamos la variable de control del while
        juega=F
      }
    }
    
    
  }
}

