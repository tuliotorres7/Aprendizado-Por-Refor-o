library(gWidgets)
library(gWidgets2)
library(gWidgets2RGtk2)



fExecutar <- function(h,...) {

   m <- matrix(c(0,0,0,0,0,0,0,0,0,0,
                0,2,1,1,0,0,1,1,1,0,
                0,1,1,1,0,0,1,1,1,0,
                0,1,1,1,1,1,1,1,1,0,
                0,1,1,1,0,0,1,1,1,0,
                0,1,1,1,0,0,1,1,1,0,
                0,1,1,1,1,1,0,0,1,0,
                0,1,1,1,1,1,0,0,100,0,
                0,1,1,1,1,1,1,1,1,0,
                0,0,0,0,0,0,0,0,0,0),10,10)
	 
	alpha <-  svalue(edit_alpha)
 	alpha <- as.numeric(alpha)   
 	gamma <-  svalue(edit_gamma)
 	gamma <- as.numeric(gamma)
	e <- svalue(edit_e)
 	e<-as.numeric(e)
	itermax <-svalue(edit_itermax)
	itermax<-as.numeric(itermax)   

  q <- matrix(c(0),100,4)  #zera Q    
  Li<-2 #posiÃ§Ã£oinicial
  Ci<-2 #posiÃ§Ã£oinicial
  
  NL <-10   #numero de linhas
  NC <-10   #numero de colunas
  NS <- NL*NC  #numero de estados
  GGG <- NULL
  GG <- 0  #acertos
  NAA <- 0  #aÃµes aleatorias
  NEP <- 0  #numero de episodios
  iter <- 0 #numero de iteraÃ§Ãµes atual
  L <- Li #linha atual
  C <- Ci  #coluna atual
  Lo=Li
  Co=Ci
  
  count <- 1
  rotx <- 0
  roty <- 0

  while(iter < itermax){
    
    iter <- iter + 1
    
    s = (L-1)*NC+C #estado atual
    
    if( runif(1,0,1) < e){    #numero aleatorio de 0 a 1
      NAA <- NAA + 1
      a <- sample(1:4,1) #numero aleatorio de 1 a 4, direÃ§Ã£o que irÃ¡ andar
    }else{
      temp <- which(q[s,] == max(q[s,]))  #posiÃ§Ã£o dos melhores resultados parao estado s na matriz q
      a= temp[sample(length(temp), 1)]  #aÃ§Ã£o q sera realizada
    }
    
    
    if (a==1){ #esquerda
      C=C-1
      roty[count] <- NL-L+1
      rotx[count] <- C
    }
    if (a==2){ #cima
      L=L-1
      roty[count] <- NL-L+1
      rotx[count] <- C
    }
    if (a==3){ #baixo
      L=L+1
      roty[count] <- NL-L+1
      rotx[count] <- C
    }
    if (a==4){ #direita
      C=C+1
      roty[count] <- NL-L+1
      rotx[count] <- C
    }
    
    count = count + 1
    
    R <- -1
    if( m[L,C] == 0 ){      #bateu
      R <- -100
    }else if (m[L,C] == 100){    #chegou no lugar certo
      R=1000
      GG=GG+1
	if(GG==1){ 
		svalue(result_np1) <- as.character(iter)
		}
    }
    
    
    ss <- (L-1)*NC+C;
    
    temp <- which(q[ss,] == max(q[ss,]))
    aa= temp[sample(length(temp), 1)]
    q[s,a] = q[s,a] + alpha*(R+gamma* q[ss,aa] - q[s,a])
    
    
    
    if (R != -1) {              
      C=Ci
      L=Li
      NEP=NEP+1
      
      x <- c(1,2,3,4,5,6,7,8,9,10
             ,10,10,10,10,10,10,10,10,10,10
             ,10,9,8,7,6,5,4,3,2,1
             ,1,1,1,1,1,1,1,1,1,1)
      y <- c(1,1,1,1,1,1,1,1,1,1
             ,1,2,3,4,5,6,7,8,9,10
             ,10,10,10,10,10,10,10,10,10,10
             ,10,9,8,7,6,5,4,3,2,1)
     	
	par(mfrow=c(1,2))
      
      plot(x,y, col = "red",type = "p")
      
      z <- c(7,7,8,8)
      k <- c(3,4,3,4)
      
      points(z,k, cex = .5, col = "blue", type = "o")
      
      
      z <- c(2,2,3,3)
      k <- c(5,6,6,5)
      
      points(z,k, cex = .5, col = "blue",type = "o")
      
      z <- c(5,6,6,5)
      k <- c(5,6,5,6)
      
      
      points(z,k, cex = .5, col = "blue",type = "o")
      
      points(2,9, cex = 2 , col = "black",type = "o")
      
      
      points(8,2, cex = 2 , col = "black",type = "o")
      grid(nx = NULL, ny = NULL, col = "black", lty = "dotted")    
      
      points( rotx, roty, cex = 2 , col = "black",type = "o")
      GGG[iter]<- GG

    plot(c(1:iter),GGG,xlab = "Interações", ylab = "Acertos",type="o",cex =0.5)
    points(c(1:iter),GGG, cex = 0.5, col = "blue", type = "o")
    
      Sys.sleep(0.05)
	count <- 1
      rotx <- Li
      roty <- Ci
    }
  }

svalue(result_sucess) <- as.character(GG)
}



label_alpha <- glabel("Taxa de Aprendizado", editable=FALSE)

edit_alpha <- gedit("0.90",width = 8)

label_gamma <- glabel("Fator de Desconto", editable=FALSE)

edit_gamma <- gedit("0.9",width = 8)

label_e <- glabel("e-greedy", editable=FALSE)

edit_e <- gedit("0.05",width = 8)

label_itermax <-glabel("Interações", editable=FALSE)

edit_itermax <-gedit("500",width = 8)



label_sucess <-glabel("Sucessos", editable=FALSE)

result_sucess <- gedit("",width = 8)


label_np1 <-glabel("Numero de passos para o primeiro Sucesso", editable=FALSE)

result_np1 <- gedit("",width = 8)


label_npu <-glabel("Numero de passos no ultimo sucesso", editable=FALSE)

result_npu <- gedit("",width = 8)


label_nmaxp <-glabel("Numero maximo de passos", editable=FALSE)

result_nmaxp <- gedit("",width = 8)


label_nminp <-glabel("Numero minimo de passos", editable=FALSE)

result_nminp <- gedit("",width = 8)


label_nmedp <-glabel("Numero medio de passos", editable=FALSE)

result_nmedp <- gedit("",width = 8)

button_executar <- gbutton("Executar", handler= fExecutar,border=TRUE)

size(button_executar)<- c(100,70)




# creation of the main window
window<-gwindow("gwindow")

# creation of the main container
BigGroup<-ggroup(cont=window)

# creation of a subcontainer
group<-ggroup(horizontal=FALSE, container=BigGroup)


fparametros <- gframe("Parâmetros", container=group)
fresultados <- gframe("Resultados", container=group)

lyt <- glayout ( cont = fparametros)
lyt[ 1 , 1 ] <- label_alpha
lyt[ 1 , 2 ] <- edit_alpha
lyt[ 2 , 1 ] <- label_gamma
lyt[ 2 , 2 ] <- edit_gamma
lyt[ 3 , 1 ] <- label_e
lyt[ 3 , 2 ] <- edit_e
lyt[ 4 , 1 ] <- label_itermax
lyt[ 4 , 2 ] <- edit_itermax
sapply(lyt[ , 1],function( i ){
font( i ) <- c( weight = "bold" , color = "black" )
})

lyt <- glayout ( cont = fresultados)
lyt[ 1 , 1 ] <- label_sucess
lyt[ 1 , 2 ] <- result_sucess
lyt[ 2 , 1 ] <- label_np1
lyt[ 2 , 2 ] <- result_np1
lyt[ 3 , 1 ] <- label_npu
lyt[ 3 , 2 ] <- result_npu
lyt[ 4 , 1 ] <- label_nmaxp
lyt[ 4 , 2 ] <- result_nmaxp
lyt[ 5 , 1 ] <- label_nminp
lyt[ 5 , 2 ] <- result_nminp
lyt[ 6 , 1 ] <- label_nmedp
lyt[ 6 , 2 ] <- result_nmedp
sapply(lyt[ , 1],function( i ){
font( i ) <- c( weight = "bold" , color = "black" )
})


nb <- gnotebook( container = BigGroup, expand=TRUE)

add(fparametros,button_executar)

# adding a space for graphics to the main container
add(BigGroup, ggraphics( container = nb))
names(nb)[1] <- "Gráficos"
