m <- matrix(c(0,0,0,0,0,0,0,0,0,0,
             0,2,1,1,1,1,1,1,1,0,
             0,1,1,1,1,1,1,1,1,0,
             0,1,1,1,1,1,1,1,1,0,
             0,0,0,1,0,0,1,1,1,0,
             0,0,0,1,0,0,1,1,1,0,
             0,1,1,1,1,1,0,0,1,0,
             0,1,1,1,1,1,0,0,1,0,
             0,1,1,1,1,1,1,100,1,0,
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
    }
    if (a==2){ #cima
        L=L-1
    }
    if (a==3){ #baixo
        L=L+1
    }
    if (a==4){ #direita
        C=C+1
    }



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
}

}

