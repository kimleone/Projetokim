# KNN - PROPRIA IMPLEMENTAÇÃO

# Entradas: Valor de K, treino, teste, variavel resposta.

# Saída: classificação de cada observação na amostra de teste

# Algoritmo 

# passo 1: selecionar um elemento da amostra de teste
# passo 2: calcular a distancia entre o elemento selecionado e todos as observações da amostra de treino
# passo 3: ordenar as distancias obtidas
# passo 4: selecionar K elementos com menor distancia da observacao selecionada no passo 1
# passo 5: classificar a observação como a classe mais frequente nos K elementos com menor distancia
# passo 6: repetir os passos 1-5 para todos os elementos da amostra de teste.
# passo 7: retornar as predições de todos os elementos na amostra de teste

setwd("C:\\Users\\Kim Leone\\Dropbox\\Projeto")
anuncios = read.csv('Social_Network_Ads.csv')
anuncios <- anuncios[,3:5] #base de dados somente com as variáveis de interesse.
indice <- sample(nrow(anuncios), nrow(anuncios)*0.75) #dividindo a base de dados (para criar teste e treino)
treino <- anuncios[indice, ] 
teste <- anuncios[-indice, ]
teste["predict"] <- '-'


distancia <- function(X1,Y1,X2,Y2){  #função para calcular a distancia dos elementos
  d <- sqrt((X1-X2)^2+(Y1-Y2)^2)     #do conjunto teste em relação às do conjunto treino.
  return(d)
}
K=7
ds=NULL

for(j in 1:100){   #calcular as distancias, ordena-las (em ordem crescente), usar os K vizinhos mais proximos para prever a variável resposta
  for(i in 1:300){ 
    ds[i] <-  distancia(teste[j,1],teste[j,2],treino[i,1],treino[i,2])
  }
  d1 <- order(ds)[1:K]                        #d1 é o vetor que contêm as posições dos vizinhos mais próximos
  for(i in 1:K){
    if(mean(treino[d1,3]) < 0.5){teste[j,4] <- 0}else{teste[j,4] <- 1}   #A condicional verifica a média 
  }                                                                #da classificação dos K vizinhos mais
                                                                   # proximos para classificar a amostra teste.
}

teste

erro=0
for(i in 1:100){                    #comparando as colunas predict com Purchased consigo
  if(teste[i,3] != teste[i,4]){     #verificar quantos erros foram cometidos.
    erro=erro+1
  }}
100-erro

sum(teste[ ,3] == teste[ ,4])




# MeuKNN = function(K, treino, teste, y){
#     contas
#     return(vetor de predicao na amostra teste)
# }

# Colocar dentro da funcao MeuKNN
# Repetir a funcao varias vezes e identificar a taxa de acerto com precisão, para K = 7


anuncios <- read.csv(choose.files())
anuncios <- anuncios[,3:5] 
indice <- sample(nrow(anuncios), nrow(anuncios)*0.75)
treino <- anuncios[indice, ] 
teste <- anuncios[-indice, ]

MeuKNN = function(K,treino,teste){
  
  distancia <- function(X1,Y1,X2,Y2){ 
    d <- sqrt((X1-X2)^2+(Y1-Y2)^2)  
    return(d)
  }
  
  ds=NULL
  A = NULL
  for(j in 1:100){    
    for(i in 1:300){ 
      ds[i] <-  distancia(teste[j,1],teste[j,2],treino[i,1],treino[i,2])
    }
    d1 <- order(ds)[1:K]    
    for(i in 1:K){if(mean(treino[d1,3]) < 0.5){A[j] <- 0}else{ A[j] <- 1}}
  }
  return(A)
}

pred = MeuKNN(19, treino, teste)
sum(pred == teste[ , 3])




A = NULL
V = NULL
for (j in 1:40) {
  cat('Fazendo a simulacao numero ',j, '\n')  
  indice <- sample(nrow(anuncios), nrow(anuncios)*0.75)
  treino <- anuncios[indice, ] 
  teste <- anuncios[-indice, ]
  
  
  V = MeuKNN(7,treino,teste)
  
  
  erro=0
  for(i in 1:100){
    if(teste[i,3] != V[i]){
      erro=erro+1
    }}
  
  A[j] = 100 - erro
  
}
Taxaacerto = mean(A)
Taxaacerto
#taxa acerto = 79.97
#taxa acerto = 80.74
#taxa acerto = 80.24
#taxa acerto = 80.26
#taxa acerto = 80.17
K = 7
reps = 1000
results = NULL
for(i in 1:reps){indice <- sample(nrow(anuncios), nrow(anuncios)*0.75)
treino <- anuncios[indice, ] 
teste <- anuncios[-indice, ]

library(kknn)
knn = kknn(formula = factor(Purchased) ~ Age + EstimatedSalary, 
           train = treino, 
           test = teste,
           k = K)
pred = knn$fitted.values
pred0 = as.numeric(pred) - 1
obsv = teste[ , 3]
results[i] = sum(pred0 == obsv)
}
results
mean(results)

class(pred)

# Repetir o processo acima, para varios valores de K, e identificar o melhor K. 
# agora usando a funcao kknn
# faça um gráfico da taxa de acerto alcançada em funçao de K
library(kknn)
reps = 500
acerto = NULL

kas = seq(1, 80, by = 2)

for(K in 1:length(kas)){
  results = NULL
  for(i in 1:reps){indice <- sample(nrow(anuncios), nrow(anuncios)*0.75)
treino <- anuncios[indice, ] 
teste <- anuncios[-indice, ]


knn = kknn(formula = factor(Purchased) ~ Age + EstimatedSalary, 
           train = treino, 
           test = teste,
           k = kas[K])
pred = knn$fitted.values
pred0 = as.numeric(pred) - 1
obsv = teste[ , 3]
results[i] = mean(pred0 == obsv)
  }
  
 acerto[K] = mean(results)
cat("processando... K =", kas[K], fill = T)
}
order(acerto)



melhorK = kas[which.max(acerto)]
melhorACC = acerto[which.max(acerto)]
titulo = paste('KNN - Melhor K igual a', melhorK)
plot(kas ,acerto, type = "p", col = "blue", pch = 20,
     lty = 1, xlab = "Valores de K", ylab = "Taxa Acerto", main = titulo)
abline(v = melhorK, lty = 'dashed')
abline(h = melhorACC, lty = 'dashed')


acerto[order(acerto)]


kas[order(acerto)]

acerto[13]
#############################


# KNN para Regressão!

library(MASS)
data(Boston)
head(Boston)

dados = Boston[,13:14]
head(dados)
plot(dados)
kas <- seq(1,100, by=2)

modelolinear = lm(medv ~ lstat, data = dados)
r = range(Boston[,1])
grid = seq(r[1], r[2], by = 0.01)
griddf = data.frame(lstat = grid)
ypred = predict(modelolinear, newdata = griddf)
ypred
library(kknn)
for (K in 1:length(kas)) {
  
plot(dados)
lines(grid, ypred, type = 'l', col = 'orange', lwd = 2)

knn = kknn(formula = medv ~ lstat, 
           train = dados, 
           test = griddf,
           k = kas[K])
lines(grid, knn$fitted.values, type = 'l', col = 'red', lwd = 2)
Sys.sleep(1)
}

# fazer a implementacao propria ajustada para o problema de regressao
# descobrir qual é o melhor K para esses dados

# Sys.sleep(1)

library(MASS)
data(Boston)
dados <- Boston[,13:14]


reps = 100
erro = NULL
kas = seq(1,300,by = 2)

MeuKNN = function(K,treino,teste){
  
  distancia <- function(X1,Y1,X2,Y2){ 
    d <- sqrt((X1-X2)^2+(Y1-Y2)^2)  
    return(d)
  }
  
  ds=NULL
  A = NULL
  ax = 127
  axx=379
  for(j in 1:ax){
    for(i in 1:axx){ 
      ds[i] <-  distancia(teste[j,1],teste[j,2],treino[i,1],treino[i,2])
    }
    d1 <- order(ds)[1:K]
    e = treino[d1,2]
    A[j] = mean(e)
  }
  return(A)
}

for(K in 1:length(kas)){
  results = NULL
  for (i in 1:reps) {
indice <- sample(nrow(dados), nrow(dados)*0.75)
treino <- dados[indice, ]
teste <- dados[-indice, ]


pred = MeuKNN(kas[K], treino, teste)

obsv = teste[ , 2]

results[i] = mean((pred - obsv)^2)
cat("primeiro for ... ",i, fill = T)
  }
  erro[K] = mean(results)
  cat("processando ... K = ",kas[K], fill = T)}

order(erro)
kas[3]#melhor k, K=5 

#######################################################################


##USANDO KKNN

require(kknn)
reps = 500
erro = NULL

kas = seq(1, 300, by = 2)

for(K in 1:length(kas)){
  results = NULL
  for(i in 1:reps){
    indice <- sample(nrow(dados), nrow(dados)*0.7)
    treino <- dados[indice, ]
    teste <- dados[-indice, ]


    knn = kknn(formula = medv ~ lstat,
               train = treino,
               test = teste,
               k = kas[K])


    pred = knn$fitted.values
    obsv = teste[ , 2]
    results[i] = mean((pred - obsv)^2)# === erro quadratico médio

  } 
  erro[K] = mean(results)
  cat("processando... K =", kas[K], fill = T)
}


erro[15]
kas[15]
##melhor K = 29.

# PLOT COM O MELHOR K 

plot(dados)
lines(grid, ypred, type = 'l', col = 'orange', lwd = 2)

knn = kknn(formula = medv ~ lstat, 
           train = dados, 
           test = griddf,
           k = 45)
lines(grid, knn$fitted.values, type = 'l', col = 'red', lwd = 2)


knn = kknn(formula = medv ~ lstat, 
           train = treino, 
           test = teste,
           k = 45)
pred = knn$fitted.values
obsv = teste[ , 2]

eqm = mean((pred - obsv)^2)

####
# No Boston Dataset, ha muito mais variaveis para se utilizar
# Agora, descobrir qual é o melhor ajuste KNN (qual o melhor k do knn) quando utilizado todas as variaveis disponiveis??
# Compare os erros quadraticos médios EQM obtidos considerando diferentes conjuntos de variaveis



library(MASS)
dados = Boston
head(dados)
indice <- sample(nrow(dados), nrow(dados)*0.7)
treino <- dados[indice, ]
teste <- dados[-indice, ]




library(kknn)
reps = 500
erro = NULL

kas = seq(1, 300, by = 2)


for(K in 1:length(kas)){
  results = NULL
  for (i in 1:reps) {
    indice <- sample(nrow(Boston), nrow(Boston)*0.7)
    treino <- Boston[indice,]
    teste <- Boston[-indice,]
    
  
  
knn <- kknn(formula = medv ~.,
            train = treino,
            test = teste,
            k = kas[K])

pred = knn$fitted.values
obsv = teste[ , 14]
results[i] = mean((pred - obsv)^2)# === erro quadratico médio

  }
  
erro[K] = mean(results)
cat("processando... K =", kas[K], fill = T)
}

order(erro)

kas[3]
#Usando todas as variaveis o menor eqm foi o calculado com K = 5

knn <- kknn(formula = medv ~., train = treino,
            test = teste, k=5)


pred = knn$fitted.values
obs = teste[,14]
eqm = mean((pred - obsv)^2) #26.16714



knn = kknn(formula = medv ~ lstat + black + ptratio + tax + nox + zn + crim,
           train = treino,
           test = teste,
           k = 5)

pred = knn$fitted.values

eqm = mean((pred - obsv)^2) ### 24.07571 ### MENOR EQM!


knn = kknn(formula = medv ~ lstat + black + zn + crim,
           train = treino,
           test = teste,
           k = 5)

pred = knn$fitted.values

eqm = mean((pred - obsv)^2) ###25.63357

knn = kknn(formula = medv ~ lstat + black + ptratio,
           train = treino,
           test = teste,
           k = 5)

pred = knn$fitted.values
obsv = teste[ , 2]

eqm = mean((pred - obsv)^2) #28.39225



knn = kknn(formula = medv ~ lstat,
           train = treino,
           test = teste,
           k = 5)


pred = knn$fitted.values
eqm = mean((pred - obsv)^2) ###k=45 (35.67689)### k=5 (41.24753)
