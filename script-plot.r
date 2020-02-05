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


q <- matrix(c(0),100,4)  #zera Q    
Li<-2 #posiçãoinicial
Ci<-2 #posiçãoinicial


NL <-10   #numero de linhas
NC <-10   #numero de colunas
NS <- NL*NC  #numero de estados
   


alpha <- 0.9   #
gamma <- 0.9     #
e <- 0.05     #Guloso
itermax <-5000   #iterações maximo

  
GG <- 0  #acertos
NAA <- 0  #aões aleatorias
NEP <- 0  #numero de episodios
iter <- 0 #numero de iterações atual
L <- Li #linha atual
C <- Ci  #coluna atual
Lo=Li
Co=Ci





count <- 1
rotx <- 0
roty <- 0



while(iter < itermax){
  

  
iter = iter + 1

s = (L-1)*NC+C #estado atual


if( runif(1,0,1) < e){    #numero aleatorio de 0 a 1
    NAA <- NAA +1
    a <- sample(1:4,1) #numero aleatorio de 1 a 4, direção que irá andar
}else{
    temp <- which(q[s,] == max(q[s,]))  #posição dos melhores resultados parao estado s na matriz q
    a= temp[sample(length(temp), 1)]  #ação q sera realizada
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
#Sys.sleep(1) 

count <- 1
rotx <- 0
roty <- 0


}

}

