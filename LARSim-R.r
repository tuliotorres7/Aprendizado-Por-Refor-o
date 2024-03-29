library(gWidgets2)
library(gWidgets2RGtk2)
library(igraph)
library(clue)

options(guiToolkit="RGtk2")


fButton <- function(h,...) {

if(svalue(rb)== "CC"){
visible(window)<- FALSE
GGG<-NULL
block25<- NULL
momento<- NULL
itermax<- 0

fExportar <- function(h,...) {

jpeg(filename = "Rplot.jpg", width = 480, height = 480, 
     units = "px", pointsize = 12, quality = 100,
     bg = "white",  res = NA, restoreConsole = TRUE)

par(mfrow=c(2,2))

plot(c(1:25),GGG,xlab = "Episódio", ylab = "Acertos",type="o",cex =  ,col="blue")
points(c(1:25),GGG, cex = 0.5, col = "blue", type = "o")



plot(c(1:(25/25)),block25,xlab = "Episódio", ylab = "Acertos",type="o",cex =0.5)
points(c(1:(25/25)),block25, cex = 0.5, col = "blue", type = "o")

plot(g,E(g), main= "MAPA")

plot(c(1:25),momento,xlab = "Episódio", ylab = "Acertos")
points(c(1:25),momento, cex = 0.5, col = "blue", type = "o")


dev.off()
}

fExecutar <- function(h,...) {

#dispose(nb,fimage)

i <- 1 # iterador
VerImpares <- 0

cont <- 0
cont2 <- 0
gimpar <- 0

if(svalue(rbg)== "Grafo Exemplo"){
  m <- read.table(row.names=1,file = "/Users/Tulio/Desktop/matrix.csv",header = TRUE, sep=',')
}else{if(svalue(rbg) == "Importe o seu Grafo"){
  m <- read.table(row.names=1,file = file.choose(),header = TRUE, sep=',')
}
}

n <- length(m) # numero de vertices
m <- as.matrix(m)

g <- graph.adjacency(m, mode ="undirected",weighted = TRUE)

round(E(g)$weight,3)


dist <- distances(g)
grau <- degree(g)

while(i <= n){  #descobrir o numero de vertices de grau impar
  if(grau[i] %% 2 == 1){
    gimpar <- gimpar + 1 
    VerImpares[gimpar] <- i  # primeira posi????o no vetor ?? 1 nao 0
  }
  i <- i + 1
}
i <- 0
matDist <- matrix(c(0),ncol=n,nrow = n)

for(i in VerImpares){
  for(u in VerImpares){
    matDist[i,u] <- dist[i,u] 
  }  
}
g <- graph.adjacency(matDist, mode ="undirected",weighted = TRUE)

x <- gimpar

for(i in n:1){
  if( i != VerImpares[x]){
    matDist <- matDist[(-i),]
    matDist <- matDist[,(-i)]
  }else{
    x<- x-1 
    if(x==0){
      x=1 # nao deixar o iterador chegar a 0
    }
  }
}

diag(matDist) = 1000

g <- graph.adjacency(m, mode ="undirected",weighted = TRUE)

t <- solve_LSAP(matDist, maximum = FALSE)

t <- as.vector(t)

matDist<- as.matrix(matDist)

for( i in 1:gimpar){
  if(is.na(VerImpares[i]) == FALSE){
    g <- add_edges(g, c(VerImpares[i],VerImpares[t[i]]))
    
    E(g)$weight[length(E(g)$weight)]<-matDist[i,t[i]]
    
    VerImpares[t[i]] = NA
  }
}

nArestas = length(E(g))
i<-1

arestas <- E(g)

length(arestas)


i<-1


tf <- which_multiple(g, arestas)
for(i in i:length(arestas)){
  if(tf[i]){
    arestas <- arestas[-i]
  }
}



q <-matrix(c(0),n, length(arestas))  #zera Q  


#vertices <- c("a","b","c","d","e","f","g")
vertices <- row.names(m)

rownames(q) <- vertices

	alpha <-  svalue(edit_alpha)
 	alpha <- as.numeric(alpha)   
 	gamma <-  svalue(edit_gamma)
 	gamma <- as.numeric(gamma)
	e <- svalue(edit_e)
 	e<-as.numeric(e)
	itermax <-svalue(edit_itermax)
	itermax <-as.numeric(itermax)   

gg<-g
iter<-0

possiveis <- E(g)
GGG<-NULL
block25 <- NULL

while(iter < itermax){
  
  iter <- iter+1
  possiveis<-NULL
  caminho <- NULL
  a<-NULL
  possiveis <- E(g)
  
  caminho <- names(tail_of(g,possiveis[1]))
  
  x<-0
  
  while(length(possiveis) != 1){
    
    s<- caminho[length(caminho)]
    
    if( runif(1,0,1) < e){    #numero aleatorio de 0 a 1
     
      p<- NULL
      ##encontra possição no vet aresta
      for(i in 1:length(possiveis)){
        if(is.na(match('TRUE',arestas==possiveis[i]))){
          for(j in 1:length(arestas)){
            if((tail_of(g,arestas[j]) == tail_of(g,possiveis[i]) ) && (head_of(g,arestas[j]) == head_of(g,possiveis[i]))){
              p[length(p) + 1] <- j
            }
          }
        }else{
          p[length(p) + 1] <- match('TRUE',arestas == possiveis[i])
        }
      }
      
      pos <- sample(p,1)
      
      a <- arestas[pos]
      
      
      troca<-NULL
      if(names(head_of(g,a)) == s){
      caminho[length(caminho)+1] <- names(tail_of(g,arestas[pos]))
      troca <- TRUE
      }else{
        caminho[length(caminho)+1] <- names(head_of(g,arestas[pos]))
      troca<-FALSE
        }
      
      ##encontra possição no vet possiveis
      if(is.na(match('TRUE',possiveis == arestas[pos]))){
      for(i in 1:length(possiveis)){
            if((tail_of(g,arestas[pos]) == tail_of(g,possiveis[i]) ) && (head_of(g,arestas[pos]) == head_of(g,possiveis[i]))){
              possiveis <- possiveis[-i]
              break
            }
        }
        }else{
          possiveis <- possiveis[-match('TRUE',possiveis == arestas[pos])]
        }
    }else{
      
      p<- NULL
      
      ##encontra possição no vet aresta, das arestas que ainda sao possiveis
      for(i in 1:length(possiveis)){
        if(is.na(match('TRUE', arestas == possiveis[i] ))){
          for(j in 1:length(arestas)){
            if((tail_of(g,arestas[j]) == tail_of(g,possiveis[i]) ) && (head_of(g,arestas[j]) == head_of(g,possiveis[i]))){
              p[length(p) + 1] <- j
            }
          }
        }else{
          p[length(p) + 1] <- match('TRUE',arestas == possiveis[i])
        }
      }
      
      temp <- which(q[s,p] == max(q[s,p]))  #posição dos melhores resultados parao estado s na matriz q
      
      pos <- p[sample(temp, 1)] #ação q será realizada
      a <- arestas[pos]
      
      
      troca<-NULL
      if(names(head_of(g,a)) == s){
        caminho[length(caminho)+1] <- names(tail_of(g,arestas[pos]))
        troca <- TRUE
      }else{
        caminho[length(caminho)+1] <- names(head_of(g,arestas[pos]))
        troca<-FALSE
      }
      
      if(is.na(match('TRUE',possiveis == arestas[pos]))){
        for(i in 1:length(possiveis)){
          if((tail_of(g,arestas[pos]) == tail_of(g,possiveis[i]) ) && (head_of(g,arestas[pos]) == head_of(g,possiveis[i]))){
            possiveis <- possiveis[-i]
            break
          }
        }
      }else{
        possiveis <- possiveis[-match('TRUE',possiveis == arestas[pos])]
      }
    }
    
    R <- -1
    
    
    
    if((s != names(tail_of(g,a))) && (troca != TRUE)){      #nao tem conexao
      R <- -10000
      x <- x + 1
    }else {
      R <- 10000
    }
    
      ss <- caminho[length(caminho)];
  
    p<- NULL 

    for(i in 1:length(possiveis)){
      if(is.na(match('TRUE',arestas==possiveis[i]))){
        for(j in 1:length(arestas)){
          if((tail_of(g,arestas[j]) == tail_of(g,possiveis[i]) ) && (head_of(g,arestas[j]) == head_of(g,possiveis[i]))){
            p[length(p) + 1] <- j
          }
        }
      }else{
        p[length(p) + 1] <- match('TRUE',arestas == possiveis[i])
      }
    }
    
    temp <- which(q[ss,p] == max(q[ss,p]))  
    
    pos <- p[sample(temp, 1)] #a????o q sera realizada
    
    aa <- arestas[pos]
    
    
    k<- ( (is.na(match('TRUE',arestas==a)))||(is.na(match('TRUE',arestas==aa))))
    if(k){
      for(i in 1:length(arestas)){
        if((tail_of(g,arestas[i]) == tail_of(g,a) ) && (head_of(g,arestas[i]) == head_of(g,a))){
          ai <- i
        }
        if((tail_of(g,arestas[i]) == tail_of(g,aa) ) && (head_of(g,arestas[i]) == head_of(g,aa))){
          aaii <- i
        }
      }
      q[s,ai] = q[s,ai] + alpha*(R+gamma * q[ss,aaii] - q[s,ai])
    }else{
      q[s,match('TRUE',arestas==a)] = q[s,match('TRUE',arestas==a)] + 
      alpha*(R+gamma * q[ss,match('TRUE',arestas==aa)] - q[s,match('TRUE',arestas==a)])
    }
    
  }
  possiveis
  caminho
  par(mfrow=c(2,2))
  GGG[iter]<- x

  plot(c(1:iter),GGG+1,xlab = "Episódios", ylab = "Acertos",type="o",cex = 0.5 ,col="blue")
  cont <- cont + x

  if(iter %% 25 == 0){
    block25[length(block25)+1]<- cont
    cont <- 0
  }
  
  }
q

plot(c(1:(itermax/25)),block25/25,xlab = "Blocos com 25 Episódios", ylab = "Média de Acertos",type="o",cex = 0.5 ,col="blue")
points(c(1:(itermax/25)),block25/25, cex = 0.5, col = "blue", type = "o")

plot(g,main = "MAPA")
  

cont2<-0
momento <- NULL
for( i in GGG){
  momento[length(momento)+1]<- cont2
  if (i ==(nArestas-1) ){
    cont2<- cont2 + 1
  }
}

plot(c(1:itermax),momento,xlab = "Episódios", ylab = "Acertos", cex = 0.5, col = "blue", type = "o")
points(c(1:itermax),momento, cex = 0.5, col = "blue", type = "o")

svalue(result_np1) <- as.character(match((nArestas-1),GGG))

svalue(result_sucess) <- as.character(length(which( (nArestas-1) == GGG)))

svalue(result_nmeda) <- as.character(sum(GGG)/itermax)


}



label_alpha <- glabel("Taxa de Aprendizado", editable=FALSE)
edit_alpha <- gedit("0.75",width = 8)

label_gamma <- glabel("Fator de Desconto", editable=FALSE)
edit_gamma <- gedit("0.15",width = 8)

label_e <- glabel("e-greedy", editable=FALSE)
edit_e <- gedit("0.05",width = 8)

label_itermax <-glabel("Episódio", editable=FALSE)
edit_itermax <-gedit("1000",width = 8)

button_executar <- gbutton("Executar", handler= fExecutar)
size(button_executar)<- c(70,40)



label_sucess <-glabel("Sucessos", editable=FALSE)

result_sucess <- gedit("",width = 8)


label_np1 <-glabel("Numero de episódios para o primeiro Sucesso", editable=FALSE)

result_np1 <- gedit("",width = 8)


label_nmaxp <-glabel("Numero maximo de passos", editable=FALSE)

result_nmaxp <- gedit("",width = 8)

label_nmeda <-glabel("Média de arestas escolhidas corretamente", editable=FALSE)

result_nmeda <- gedit("",width = 8)

button_exportar <- gbutton("Exportar", handler= fExportar)

#size(button_exportar)<- c(70,40)



# creation of the main window
window2<-gwindow("LARSim-R PCC", visible = FALSE)
visible(window2)<- TRUE
# creation of the main container
BigGroup<-ggroup(cont=window2)

# creation of a subcontainer
group<-ggroup(horizontal=FALSE, container=BigGroup)


fparametros <- gframe("Parâmetros", container=group)
fgrafo <- gframe("Grafo", container=group)
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
lyt[ 5 , 2 ] <- button_executar

sapply(lyt[ , 1],function( i ){
#font( i ) <- c( weight = "bold" , color = "black" )
})


tipoGrafo<- c("Grafo Exemplo", "Importe o seu Grafo")
rbg <- gradio(tipoGrafo, container = fgrafo)


lyt <- glayout ( cont = fresultados)
lyt[ 1 , 1 ] <- label_sucess
lyt[ 1 , 2 ] <- result_sucess
lyt[ 2 , 1 ] <- label_np1
lyt[ 2 , 2 ] <- result_np1
#lyt[ 3 , 1 ] <- label_nmaxp
#lyt[ 3 , 2 ] <- result_nmaxp
lyt[ 4 , 1 ] <- label_nmeda
lyt[ 4 , 2 ] <- result_nmeda
lyt[ 5 , 2 ] <- button_exportar

sapply(lyt[ , 1],function( i ){
#font( i ) <- c( weight = "bold" , color = "black" )
})

# adding a space for graphics to the main container
g1<-ggraphics(container = BigGroup , expand = TRUE)
add(BigGroup,g1)


dispose(window)

visible(window2) <- TRUE



#nb <- gnotebook(container = BigGroup, expand=TRUE)	
#add(BigGroup, ggraphics( container = nb))
#fimage <- gframe( "",container = nb[1])
#img <- gimage(filename = "img.png",cont = fimage)
#names(nb)[1] <- "Gráficos"
#delete(nb[1],fimage)






















}else{if(svalue(rb) == "NAV"){


fButton_exportar <- function(h,...) {}


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
  vetPassos<-NULL
  while(iter < itermax){
    
    iter <- iter + 1

    R <- -1
	passos <-0
	while(R == -1) { 
	
            passos<-passos+1
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
              svalue(result_npu)  <- as.character(passos)  
              }
              }
              
              ss <- (L-1)*NC+C;
              
              temp <- which(q[ss,] == max(q[ss,]))
              aa= temp[sample(length(temp), 1)]
              q[s,a] = q[s,a] + alpha*(R+gamma* q[ss,aa] - q[s,a])
    
    }             
      C=Ci
      L=Li
      NEP=NEP+1
      vetPassos[iter]<- passos

      x <- c(1,2,3,4,5,6,7,8,9,10
             ,10,10,10,10,10,10,10,10,10,10
             ,10,9,8,7,6,5,4,3,2,1
             ,1,1,1,1,1,1,1,1,1,1)
      y <- c(1,1,1,1,1,1,1,1,1,1
             ,1,2,3,4,5,6,7,8,9,10
             ,10,10,10,10,10,10,10,10,10,10
             ,10,9,8,7,6,5,4,3,2,1)
     	
	par(mfrow=c(1,2))
      
      plot(x,y, col = "red",type = "o")
      
      #z <- c(7,7,8,8)
      #k <- c(3,4,3,4)
	
	z <- c(3,3,4,4,3)
      k <- c(7,8,8,7,7)
      

      points(k,z, cex = 2, col = "blue", type = "o")
      
      
      z <- c(2,2,3,3,2)
      k <- c(5,6,6,5,5)
      
      points(z,k, cex = 2, col = "blue",type = "o")
      
      z <- c(5,5,6,6,5)
      k <- c(5,6,6,5,5)
      
      
      points(z,k, cex = 2, col = "blue",type = "o")
      
      points(2,9, cex = 2 , col = "Green",type = "o",pch =19)
      
      
      points(8,2, cex = 2 , col = "Yellow",pch = 19, type = "o")
      grid(nx = NULL, ny = NULL, col = "black", lty = "dotted")    
      
      points( rotx, roty, cex = 2 , col = "black",type = "o")
      GGG[iter]<- GG

    plot(c(1:iter),GGG,xlab = "Episódio", ylab = "Acertos",type="o",cex =0.5)
    points(c(1:iter),GGG, cex = 0.5, col = "blue", type = "o")
    
      Sys.sleep(0.05)
	count <- 1
      rotx <- Li
      roty <- Ci

svalue(result_sucess) <- as.character(GG)
svalue(result_nmedes) <- as.character(itermax/GG)  
svalue(result_nmaxp)  <- as.character(NAA)


  }


}



label_alpha <- glabel("Taxa de Aprendizado", editable=FALSE)

edit_alpha <- gedit("0.90",width = 8)

label_gamma <- glabel("Fator de Desconto", editable=FALSE)

edit_gamma <- gedit("0.9",width = 8)

label_e <- glabel("e-greedy", editable=FALSE)

edit_e <- gedit("0.05",width = 8)

label_itermax <-glabel("Episódios", editable=FALSE)

edit_itermax <-gedit("500",width = 8)



label_sucess <-glabel("Sucessos", editable=FALSE)

result_sucess <- gedit("",width = 8)


label_np1 <-glabel("Numero de Episódios para o primeiro Sucesso", editable=FALSE)

result_np1 <- gedit("",width = 8)


label_npu <-glabel("Numero de passos no primeiro episódio de sucesso", editable=FALSE)

result_npu <- gedit("",width = 8)


label_nmaxp <-glabel("Numero de açoes aleatórias", editable=FALSE)

result_nmaxp <- gedit("",width = 8)


label_nminp <-glabel("Numero minimo de passos/ações para o sucesso", editable=FALSE)

result_nminp <- gedit("13",width = 8)


label_nmedp <-glabel("Numero medio de passos/ações", editable=FALSE)

result_nmedp <- gedit("17",width = 8)


label_nmedes <-glabel("Número médio de episódios por sucesso", editable=FALSE)

result_nmedes <- gedit("",width = 8)


button_executar <- gbutton("Executar", handler= fExecutar)

size(button_executar)<- c(100,70)


button_exportarNAV <- gbutton("Exportar",handler = fButton_exportar)



# creation of the main window
window3<-gwindow("LARSim-R NAV",visible= FALSE)

# creation of the main container
BigGroup<-ggroup(cont=window3)

# creation of a subcontainer
group<-ggroup(horizontal=FALSE, container=BigGroup)


fparametros <- gframe("Parâmetros", container=group)
fresultados <- gframe("Resultados", container=group)
fmatriz <- gframe("Matriz", container = group)

lyt <- glayout ( cont = fparametros)
lyt[ 1 , 1 ] <- label_alpha
lyt[ 1 , 2 ] <- edit_alpha
lyt[ 2 , 1 ] <- label_gamma
lyt[ 2 , 2 ] <- edit_gamma
lyt[ 3 , 1 ] <- label_e
lyt[ 3 , 2 ] <- edit_e
lyt[ 4 , 1 ] <- label_itermax
lyt[ 4 , 2 ] <- edit_itermax
lyt[ 5 , 2 ] <- button_executar
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
lyt[ 7 , 1 ] <- label_nmedes
lyt[ 7 , 2 ] <- result_nmedes
lyt[ 8 , 2 ] <- button_exportarNAV
sapply(lyt[ , 1],function( i ){
font( i ) <- c( weight = "bold" , color = "black" )
})



tipoMatriz<- c("Matriz Exemplo", "Importe a sua Matriz")
rbm <- gradio(tipoMatriz, container = fmatriz)


nb <- gnotebook( container = BigGroup, expand=TRUE)
# adding a space for graphics to the main container
add(BigGroup, ggraphics( container = nb))
names(nb)[1] <- "Gráficos"





dispose(window)


visible(window3)<- TRUE

}else{  }}
}

# creation of the main window

window <-gwindow("LARSim-R")

# creation of the main container
BigGroup<-ggroup(cont=window)

# creation of a subcontainer
group<-ggroup(horizontal=FALSE, container=BigGroup)

fmodulos <- gframe("Modulos", container=group)
size(fmodulos)<- c(300,300)

modulo <- c("CC","NAV","Estatistico")
rb <- gradio(modulo, container= fmodulos)

button_executar <- gbutton("Executar",handler = fButton, container = group)
size(button_executar)<- c(40,50)
svalue(rb)<-"NAV"
fimage <- gframe("Realização", container= BigGroup)
img <- gimage(filename = "C:\\Users\\Tulio\\Desktop\\RLARSim\\img.png",cont = fimage)
visible(window)<- TRUE
#source("C:\\Users\\Tulio\\Desktop\\RLARSim\\img.png")
