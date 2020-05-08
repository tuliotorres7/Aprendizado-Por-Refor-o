library(igraph)
library(clue)


n <- 7 # numero de vertices
i <- 1 # iterador
VerImpares <- 0

gimpar <- 0
m <- read.table(row.names=1,file = "/home/tulio/Área de Trabalho/amatrix.csv",header = TRUE, sep=',')

m <- as.matrix(m)

g <- graph.adjacency(m, mode ="undirected",weighted = TRUE)

round(E(g)$weight,3)


plot(g,edge.label=round(E(g)$weight, 3))
  
dist <- distances(g)
grau <- degree(g)

while(i <= n){  #descobrir o numero de vertices de grau impar
  if(grau[i] %% 2 == 1){
    gimpar <- gimpar + 1 
    VerImpares[gimpar] <- i  # primeira posição no vetor é 1 nao 0
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

plot(g,edge.label=round(E(g)$weight,3))
matDist<- as.matrix(matDist)

for( i in 1:gimpar){
  if(is.na(VerImpares[i]) == FALSE){
    g <- add_edges(g, c(VerImpares[i],VerImpares[t[i]]))
    
    E(g)$weight[length(E(g)$weight)]<-matDist[i,t[i]]
    
    VerImpares[t[i]] = NA
    plot(g)
  }
}

plot(g,edge.label=round(E(g)$weight,3))

#tkplot(g,edge.label=round(E(g)$weight,3))



narestas = length(E(g)$weight)
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


n <-7

q <-matrix(c(0),n, length(arestas))  #zera Q  
q

vertices <- c("a","b","c","d","e","f","g")
rownames(q) <- vertices



tail_of(g,arestas[[5]])
head_of(g,arestas[[5]])


alpha <- 0.8
gamma <- 0.8
e <- 0.09       #Guloso
itermax <- 300   #iterações maximo

gg<-g
iter<-0

narestas <- 5
#    if((tail_of(g,arestas[i])==tail_of(g,arestas[9]) )&&(head_of(g,arestas[12])==head_of(arestas[9]))){}

length(arestas) 


possiveis <- E(g)


while(iter < itermax){

  iter <- iter+1
  possiveis<-NULL
  caminho <- NULL
  possiveis <- E(g)
  
  a <- possiveis[1]
  caminho <- names(tail_of(g,possiveis[1]))
 
  
    while(length(possiveis) != 0){
      #s <- tail_of(g,a)
      s <- a
      if( runif(1,0,1) < e){    #numero aleatorio de 0 a 1
      #vertices[-match('TRUE', letters==v)] impossibilita ficar no mesmo estado
        del <- sample(possiveis,1)#seleciona aleatoriamente o próximo vertice 
        a <- possiveis[del]
        caminho[length(caminho)+1] <- names(tail_of(g,possiveis[a]))
        possiveis <- possiveis[-del]
        #a <- sample(vertices[-match('TRUE', letters==s)],1)
          }else{
            
            p<- NULL
            
            for(i in 1:length(possiveis)){
              if(is.na(match('TRUE',arestas==possiveis[i]))){
                for(j in 1:length(arestas)){
                  if((tail_of(g,arestas[j]) == tail_of(g,possiveis[i]) ) && (head_of(g,arestas[j]) == head_of(g,possiveis[i]))){
                    p[length(p) + 1] <- arestas[j]
                  }
                }
              }else{
                p[length(p) + 1] <- match('TRUE',arestas==possiveis[i])
              }
            }
            
        temp <- which(q[tail_of(g,s),p] == max(q[tail_of(g,s),p]))  #posição dos melhores resultados parao estado s na matriz q
        a <- temp[sample(length(temp), 1)] #ação q sera realizada
        caminho[length(caminho)+1] <- names(tail_of(g,possiveis[a]))
        a <- possiveis[a]
        possiveis <- possiveis[-a]
        #a <- names(a)
        }

      R <- -1
      
      if(head_of(g,s) != tail_of(g,a)){      #nao tem conexao
        R <- -100
      }else {
        R <- 700
        }
      
      ss <- a;
      
      p<- NULL 
      
      for(i in 1:length(possiveis)){
      if(is.na(match('TRUE',arestas==possiveis[i]))){
        for(j in 1:length(arestas)){
          if((tail_of(g,arestas[j]) == tail_of(g,possiveis[i]) ) && (head_of(g,arestas[j]) == head_of(g,possiveis[i]))){
            p[length(p) + 1] <- arestas[j]
          }
        }
      }else{
        p[length(p) + 1] <- match('TRUE',arestas== possiveis[i])
      }
      }
      
      temp <- which(q[tail_of(g,ss),p] == max(q[tail_of(g,ss),p]))  
      q
      
      aa <- temp[sample(length(temp), 1)] #ação q sera realizada
      aa <- possiveis[aa]
      
      caminho
      possiveis
      a
      aa
      k<- ( (is.na(match('TRUE',arestas==a)))||(is.na(match('TRUE',arestas==aa))))
      if(k){
        for(i in 1:length(arestas)){
          if((tail_of(g,arestas[i]) == tail_of(g,a) ) && (head_of(g,arestas[i]) == head_of(g,a))){
            ii <- i
          }
          if((tail_of(g,arestas[i]) == tail_of(g,aa) ) && (head_of(g,arestas[i]) == head_of(g,aa))){
            uu <- i
          }
        }
        q[tail_of(g,s),ii] = q[tail_of(g,s),ii] + alpha*(R+gamma * q[tail_of(g,ss),uu] - q[tail_of(g,ss),ii])
      }else{
        q[tail_of(g,s),match('TRUE',arestas==a)] = q[tail_of(g,s),match('TRUE',arestas==a)] + alpha*(R+gamma * q[tail_of(g,ss),match('TRUE',arestas==aa)] - q[tail_of(g,ss),match('TRUE',arestas==a)])
        }
      q
    }
        possiveis
        caminho
        q
}

possiveis

