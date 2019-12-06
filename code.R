---
title: "classification non supervisee/supervisée et sa stabilité"
author: "Triki Sadok"
date: "8 October 2019"
output: pdf_document 
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(fig.height = 3,echo = TRUE)
```

 \center { Predicting player's positions by their scores from FIFA 2018}
\newpage

# I-INTRODUCTION
Football is the most reputaded sport in the planet, followers are from all ages and sexes.
Many people and start ups want to study the profiling of the players for some reason. 
To facilitate this task, we will classify players aith some critera to predict each player's position in the game. 

# 1-Questions to answer:
-What is the criteria we will use to classify players? s'il est "Attacker", "Defender","goal keeper" ou "midfielder"

-how to classify a new player from it's caracteristics?

- will this classification be stable ? 

## 2-Goals:
The goal is to find the best classes, or segments, where we will divide players into homogenious classes and then caracterise those classes.

# II-Data manipulation:

```{r }
base=read.csv('file.csv')
a2=base[,-c(1,2,3,4,8,9,10)]
head(base)

```

Ce jeu de données est celui du jeu Fifa 2017 contenant tous les joueurs inscrits dans l'organisme FIFPRO 
## Statistique Descreptive : 
```{r}
base::summary(a2[,1:10])
```

# III-NON SUPERVISED CLASSIFICATION

## 1-K-means
```{r echo=FALSE}
classifK_means<-function(X,k){
  classif<-kmeans(X, k, iter.max = 10, nstart = 1)
  a=rep(0,nrow(X))
  X=cbind.data.frame(X,a)
  
  X[,48]=as.factor(classif$cluster)
  return(X)
}
```

```{r  echo=FALSE}
library(ggplot2)
base=read.csv('file.csv')
a2=base[,-c(1,2,3,4,8,9,10)]
a2=a2[-14]
a1=a2[1:300,]
kmeans=classifK_means(a1,4)

library(plyr)
b=revalue(kmeans$a, c("1"="goal_keeper", "2"="Defender","3"="Miedfielder","4"="Attacker"))
P=cbind.data.frame(base$name[1:300],base$club[1:300],b)
colnames(P)[3]='classe'
head(P)



```

## Results: 

```{r}
ggplot(kmeans, aes(kmeans$overall, kmeans$age, color = P$classe)) + geom_point()
```

## 2-Hierarchical classification:

```{r  echo=FALSE}
base=read.csv('file.csv')
a2=base[,-c(1,2,3,4,8,9,10)]
a2=a2[-14]
a1=a2[1:1000,]

dd<-c("euclidian")
aggr<-c("ward","weighted","single","complete","average","flexible","gaverage") 

classif_hc<-function(X,k,aggr,dd){
  library(cluster)
  classif<-agnes(X,method = aggr) 
  #d<-dist(scale(X),method =dd)
  #classif<-hclust(d,method = aggr)
  cut<-cutree(classif,k=k)
  a=rep(0,nrow(X))
  X=cbind.data.frame(X,a)
  
  X[,48]=as.factor(cut)
  return(X)
}

hierar=classif_hc(a1,4,aggr[1],dd[1])
b2=revalue(hierar$a, c("1"="Attacker", "2"="Goal_keeper","3"="Midfielder","4"="Defender"))

P2=cbind.data.frame(base$name[1:1000],base$club[1:1000],b2)
colnames(P2)[3]='classe'
head(P2)

```
## Representation : 
```{r}
ggplot(hierar, aes(hierar$overall, hierar$age, color =P2$classe )) + geom_point()
```

## 3 Final classification of players:

```{r  echo=FALSE}
bs=cbind.data.frame(P[1:300,],P2$classe[1:300])
colnames(bs)[3]="Class Kmeans"
colnames(bs)[4]= "Class cluster"
head(bs)
```

-The best is to classify players to 4 classes.

-Those 4 classes will be: 

- 1: **Attacker**

- 2: **Goal keeper**

- 3: **Midfielder**

- 4: **Defender**

# IV-INDICE DE RAND ET STABILITE

## 1-Définition
"Rand" Index is a tool that can measure the **similarity** between two classifications.
He is mostly used in automatic categorisation.
It can mesure the consistancy between two possible classifications.

## 2- Algorithm(Simulation)
* Simulate a Data set X composed of multivariate normal distribution xi-> N(ui,sigma).

```{r X, echo=FALSE}
simul.X <- function(n1,n2,n3,n4){ 
  
  library(mvtnorm)
  mu=c(12,30)
  sigma=diag(1,2)
  x1=rmvnorm(n =n1, mean =mu, sigma)
  mu=c(25,36)
  sigma=diag(1,2)
  x2=rmvnorm(n = n2, mean =mu, sigma)
  mu=c(11,25)
  sigma=diag(1,2)
  x3=rmvnorm(n = n3, mean =mu, sigma)
  mu=c(5,35)
  sigma=diag(1,2)
  x4=rmvnorm(n = n4, mean =mu, sigma)
  X=rbind.data.frame(x1,x2,x3,x4)
return(X)
}
X=simul.X(100,70,30,50)
head(X)

```
 

* Apply classification Algorithms A(k) et B(k) -> Pa(k) , Pb(k).
(Here, we will use Kmeans A(k) et hierarchical classification B(K)).

* Study stability of partitions using Rand index and Rand index modified.
 
* sampling (stratified, SAS ..)Ej, applyed to Aj(k) et Bj(k) -> Pa(K) , Pb(K).
 Evaluate stability with IR , IRC
 
* Repeate N times the process.

```{r }
RI <- function(D1,D2){
  
  library(fossil)
  
  v<-as.numeric(D1)
  vv<-as.numeric(D2)
  
   # rand index
  x <- abs(sapply(v, function(x) x - v))
  x[x > 1] <- 1
  y <- abs(sapply(vv, function(x) x - vv))
  y[y > 1] <- 1
  sg <- sum(abs(x - y))/2
  bc <- choose(dim(x)[1], 2)
  ri <- 1 - sg/bc
  
  # adj rand index
  a <- length(table(v))
  N <- length(v)
  ctab <- matrix(N, a, a)
  for (j in 1:a) {
    for (i in 1:a) {
      ctab[j, i] <- length(which(vv[which(v == 
                                                i)] == j))
    }
  }
  sumnij <- sum(choose(ctab, 2))
  sumai <- sum(choose(colSums(ctab), 2))
  sumbj <- sum(choose(rowSums(ctab), 2))
  Ntwo <- choose(N, 2)
  ari <- abs((sumnij - (sumai * sumbj)/Ntwo)/(0.5 * (sumai + 
                                                       sumbj) - (sumai * sumbj)/Ntwo))
  
  #a=rand.index(v,vv)
  #b=adj.rand.index(v,vv)
  A=cbind(ri,ari)
  colnames(A)=c("rand_index","adj_rand_index")
  A=as.data.frame(A)
  #A=as.table(A)
  #METHOD <- "Augmented Dickey-Fuller Test"
  #names(ari) <- "rand index"
  #names(ri) <- "adjasted-rand index"
  #structure(list(statistic = ari, parameter = ri, method = METHOD, data.name ="A" ), 
   #         class = "htest")
  return(A)
}
mean_rand11<-function(D,N,k,sampling){
  m=0
  p=0
  
  if (sampling=="stratification"){
  
    for(i in 1:N){
    kmeans=classifK_means(D,k)
    hierar=classif_hc(D,k,aggr[1],dd[1])
    strat4=stratif(hierar$a,kmeans$a)
    rand4=RI(strat$echantillon_1,strat$echantillon_2)
    m<-m+rand3$rand_index
    p<-p+rand3$adj_rand_index
    }}
  
  else if (sampling=="sample"){
    for(i in 1:N){
      
      D[sample(nrow(D), N), ]
      kmeans=classifK_means(D,k)
      hierar=classif_hc(D,k,aggr[1],dd[1])
      rand3=RI(kmeans$a,hierar$a)
      m<-m+rand3$rand_index
      p<-p+rand3$adj_rand_index
    }
    
  }
  
  mm<-m/N
  pp<-p/N
  AA=cbind(mm,pp)
  colnames(AA)=c("MEAN-RIx","MEAN-ARI")
  return(AA)
}

```

## Using Rand index for FIFA18 dataset

```{r}
mean=mean_rand11(a1,50,4,"sample")
head(mean)
```








## 3-Comparing and Interpreting:

- >*Simple sampling (SAS):*
 

- >*Stratified Sampling:*


-We notice that the mean of Rand index (after 50 itérations) is close to 1 for the two sampling methods **simple** et **stratified**. That confirm that the two methods of classification used gives almost the same classes.

-Rand index modified gives same results (=0.7) and, it confirms the similarity of the methods.
* We will the use then Kmeans and hierarchical classifications.

# V-SUPERVISED CLASSIFICATION

## 1- Decision Tree:

```{r  echo=FALSE}
base=read.csv('file.csv')
a2=base[,-c(1,2,3,4,8,9,10)]
a2=a2[-14]
a1=a2[1:1000,]

classifK_means<-function(X,k){
  classif<-kmeans(X, k, iter.max = 10, nstart = 1)
  a=rep(0,nrow(X))
  X=cbind.data.frame(X,a)
  
  X[,48]=as.factor(classif$cluster)
  return(X)
}

library(rpart)
library(rpart.plot)
arbre.full <- rpart(kmeans$a ~ ., data = kmeans, method = "class")
rpart.plot(arbre.full)
```



# VI-CONCLUSION:

-Finally, Thanks to a non supervised classification, with different methods, we have now a good partition of football international players : "Attacker", "Defender","Goal keeper" et "Midfielder".

-To verify the similarity in resulats given by Kmeans and hierarchical classification, Rand index is good, enough close to 1.


# VII-ANNEXE

```{r eval=FALSE}
#INDICE DE RAND: 
simul.X <- function(n1,n2,n3,n4){ 
  
  library(mvtnorm)
  mu=c(12,30)
  sigma=diag(1,2)
  x1=rmvnorm(n =n1, mean =mu, sigma)
  mu=c(25,36)
  sigma=diag(1,2)
  x2=rmvnorm(n = n2, mean =mu, sigma)
  mu=c(11,25)
  sigma=diag(1,2)
  x3=rmvnorm(n = n3, mean =mu, sigma)
  mu=c(5,35)
  sigma=diag(1,2)
  x4=rmvnorm(n = n4, mean =mu, sigma)
  X=rbind.data.frame(x1,x2,x3,x4)
return(X)
}

#HIERARCHICAL CLASSIFICATION

dd<-c("euclidian")
aggr<-c("ward","weighted","single","complete","average","flexible","gaverage") 

classif_hc<-function(X,k,aggr,dd){
  
  library(cluster)
  classif<-agnes(X,method = aggr) 
  cut<-cutree(classif,k=k)
  a=rep(0,nrow(X))
  X=cbind.data.frame(X,a)
  
  X[,48]=as.factor(cut)
  return(X)
}

#K-MEANS
classifK_means<-function(X,k){
  classif<-kmeans(X, k, iter.max = 10, nstart = 1)
  a=rep(0,nrow(X))
  X=cbind.data.frame(X,a)
  
  X[,48]=as.factor(classif$cluster)
  return(X)
  
}

 #INDICE DE RAND
RI <- function(D1,D2){
  
  library(fossil)
  
  v<-as.numeric(D1)
  vv<-as.numeric(D2)
  
   # rand index
  x <- abs(sapply(v, function(x) x - v))
  x[x > 1] <- 1
  y <- abs(sapply(vv, function(x) x - vv))
  y[y > 1] <- 1
  sg <- sum(abs(x - y))/2
  bc <- choose(dim(x)[1], 2)
  ri <- 1 - sg/bc
  
  # adj rand index
  a <- length(table(v))
  N <- length(v)
  ctab <- matrix(N, a, a)
  for (j in 1:a) {
    for (i in 1:a) {
      ctab[j, i] <- length(which(vv[which(v == 
                                                i)] == j))
    }
  }
  sumnij <- sum(choose(ctab, 2))
  sumai <- sum(choose(colSums(ctab), 2))
  sumbj <- sum(choose(rowSums(ctab), 2))
  Ntwo <- choose(N, 2)
  ari <- abs((sumnij - (sumai * sumbj)/Ntwo)/(0.5 * (sumai + 
                                                       sumbj) - (sumai * sumbj)/Ntwo))
  
  #a=rand.index(v,vv)
  #b=adj.rand.index(v,vv)
  A=cbind(ri,ari)
  colnames(A)=c("rand_index","adj_rand_index")
  A=as.data.frame(A)
  #A=as.table(A)
  #METHOD <- "Augmented Dickey-Fuller Test"
  #names(ari) <- "rand index"
  #names(ri) <- "adjasted-rand index"
  #structure(list(statistic = ari, parameter = ri, method = METHOD, data.name ="A" ), 
   #         class = "htest")
  return(A)
}

# STRATIFICATION
stratif <- function(D1,D2){
  library(splitstackshape)
  A=cbind.data.frame(D1,D2)
  c=stratified(A,c("D1","D2"),.2) #not sure about the size = 20%
  colnames(c)<-c("echantillon_1","echantillon_2")
  return(c)
}
```

