library(igraph)
library(clue)
i <- 1 # iterador
VerImpares <- 0

gimpar <- 0
m <- read.table(row.names=1,file = "/Users/Tulio/Desktop/matrix.csv",header = TRUE, sep=',')

n <- length(m) # numero de vertices

m <- as.matrix(m)

g <- graph.adjacency(m, mode ="undirected",weighted = TRUE)


#plot(g,edge.label=round(E(g)$weight, 3))

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

#plot(g,edge.label=round(E(g)$weight,3))

#tkplot(g,edge.label=round(E(g)$weight,3))

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

#are <- E(g)

q <-matrix(c(0),n, length(arestas))  #zera Q  

vertices <- row.names(m)


rownames(q) <- vertices

alpha <- 1
gamma <- 0.00
e <- 0.00    #Guloso
itermax <- 200 #itera????es maximo

gg<-g
iter<-0

cont <- 0
cont8<-0
possiveis <- E(g)
GGG<-NULL
block <- NULL
tamBlock <- 25
maior8 <- NULL
are<- matrix(nrow = itermax*2 , ncol = nArestas+1)
iter <- 0 

while(iter < itermax){
  iter <- iter+1
  possiveis<-NULL
  caminho <- NULL
  a<-NULL
  reforcos <-NULL
  reforcos[1]<-NA
  possiveis <- E(g)
  
  caminho <- names(tail_of(g,possiveis[1]))
  
  x<-0
  i<-nArestas
  while(length(possiveis) != 1){
    #s <- a
    
    s<- caminho[length(caminho)]
    
    if( runif(1,0,1) < e){    #numero aleatorio de 0 a 1
      #vertices[-match('TRUE', letters==v)] impossibilita ficar no mesmo estado
      
      p<- NULL
      ##encontra possiÃ§Ã£o no vet aresta
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
      
      
      
      
      
      ##encontra possiÃ§Ã£o no vet possiveis
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
      #a <- sample(vertices[-match('TRUE', letters==s)],1)
    }else{
      
      p<- NULL
      
      ##encontra possiÃ§Ã£o no vet aresta, das arestas que ainda sao possiveis
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
      
      temp <- which(q[s,p] == max(q[s,p]))  #posi????o dos melhores resultados parao estado s na matriz q
      pos <- p[sample(temp, 1)] #a????o q sera realizada
      a <- arestas[pos]
      
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
    
    
    troca<-NULL
    if(names(head_of(g,a)) == s){
      caminho[length(caminho)+1] <- names(tail_of(g,arestas[pos]))
      troca <- TRUE
    }else{
      caminho[length(caminho)+1] <- names(head_of(g,arestas[pos]))
      troca<-FALSE
    }
    
    
    if((s == names(tail_of(g,a))) || (troca == TRUE)){      #nao tem conexao
      R <- 1
      reforcos[length(reforcos)+1]<- 1
      x <- x + 1
    }else {
      R <- -10000
      reforcos[length(reforcos)+1] <- 0
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
      q[s,match('TRUE',arestas==a)] = q[s,match('TRUE',arestas==a)] + alpha*(R+gamma * q[ss,match('TRUE',arestas==aa)] - q[s,match('TRUE',arestas==a)])
    }
    caminho
    a
    possiveis
  }
  
  if(names(tail_of(g,possiveis[1])) == s )
  {
    x<-x +1
    caminho[length(caminho)+1] <- names(tail_of(g,possiveis[1]))
    reforcos[nArestas +1] <- 1
    
    }else{
    if(names(head_of(g,possiveis[1]))==s){
      x<-x +1
      caminho[length(caminho)+1] <- names(head_of(g,possiveis[1]))
      reforcos[nArestas +1] <- 1
      
        }else{
      caminho[length(caminho)+1] <- names(head_of(g,possiveis[1]))
      reforcos[nArestas +1] <- 0
        }
    }
  
    GGG[iter]<- x
 are[iter*2-1,] <- caminho
  are[iter+iter,] <- reforcos
  
  if(iter %% tamBlock == 0){
    block[length(block)+1]<- cont
    cont <- 0
    maior8[length(maior8)+1] <- cont8
    cont8 <- 0
  }
  cont <- cont + x
  
  if(x >= 8){cont8 <- cont8 +1 }
    
  if (iter > tamBlock){plot(c(1:(length(block))),block/tamBlock,xlab = "bloco de 25 interações", ylab = "media de Acertos",type="o",cex =0.5)}  
  plot(c(1:length(GGG)),GGG,xlab = "Interações", ylab = "Acertos",type="o")
  
  
  }
caminho


plot(c(1:(length(maior8))),maior8,xlab = "bloco de 25 episodios", ylab = "mais que 8",type="o",cex =0.5)
q

plot(c(1:(length(block))),block/tamBlock,xlab = "bloco de 25 episodios", ylab = "media de Acertos",type="o",cex =0.5)


sum(GGG)/length(GGG)

plot(g)
