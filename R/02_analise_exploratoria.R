##############################################################
####Tutorial de Análise Exploratória de Dados            #####
####Sara Mortara, Andrea Sánchez-Tapia & Diogo S. B. Rocha###
#### fev 2020                                           #####


#Abrir o quarteto de Anscombe, já existente no R
data("anscombe")

dim(anscombe) # dimensao dos dados, N de linhas e N de colunas
head(anscombe) # seis primeiras linhas dos dados
class(anscombe) # classe do objeto
str(anscombe) # estrutura do objeto

#Selecionando colunas dos dados
##Vamos fazer a média por das colunas com x.

mean(anscombe$x1)
mean(anscombe$x2)
mean(anscombe$x3)
mean(anscombe$x4)

#Conhecendo a função apply
##A mesma tarefa mas agora com apenas uma linha de comando usando a função apply.
?apply

### o mesmo calculo, agora apenas em 1 linha de comando
#### media de todos os vetores x
apply(anscombe[,1:4], 2, mean) #aplica uma funcao a todas as linhas de um objeto

### media de todos os vetores y
apply(anscombe[,5:8], 2, mean)

#Descrição estatística dos dados
## variância dos dados
apply(anscombe, 2, var) # aplica a funcao var a todas as linhas do objeto

# Ententendo a correlação e coeficiente de regressão dos conjuntos x e y.

## correlação
cor(anscombe$x1, anscombe$y1)
cor(anscombe$x2, anscombe$y2)
cor(anscombe$x3, anscombe$y3)
cor(anscombe$x4, anscombe$y4)

## coeficiente de regressão
### primeiro criamos objetos com as regressoes dos quatro conjuntos
m1 <- lm(anscombe$y1 ~ anscombe$x1)
m2 <- lm(anscombe$y2 ~ anscombe$x2)
m3 <- lm(anscombe$y3 ~ anscombe$x3)
m4 <- lm(anscombe$y4 ~ anscombe$x4)

### vamos criar agora uma lista com todos os modelos para facilitar o trabalho
mlist <- list(m1, m2, m3, m4)

### agora sim podemos calcular de forma menos repetitiva os coeficientes de regressao
lapply(mlist, coef)

#Os dados têm mesma média, mesma variância, mesma correlação e mesmo valores dos coeficientes (intercepto e inclinação do modelo linear). Em que os dados são diferentes?
anscombe

#Os valores parecem difentes. Mas quão diferentes?

## funcao par para definir as configuracoes da janela grafica entre em ?par
par(mfrow=c(2, 2), #abre uma janela gráfica com 2 linhas  e 2 colunas
    las=1, # deixa as legendas dos eixos na vertical
    bty="l") # tipo do box do grafico em L
plot(anscombe$y1 ~ anscombe$x1) #plot das variaveis
abline(mlist[[1]]) # adicionando a reta prevista pelo modelo de regressao
plot(anscombe$y2 ~ anscombe$x2)
abline(mlist[[2]])
plot(anscombe$y3 ~ anscombe$x3)
abline(mlist[[3]])
plot(anscombe$y4 ~ anscombe$x4)
abline(mlist[[4]])

par(mfrow=c(1, 1)) # retorna a janela grafica para o padrao de 1 linha e 1 coluna

#------------------------------------------------------------

# Parte 2: Uma rotina (entre muitas possíveis) de análise exploratória

## Padrões morfológicos de espécies de Iris
### O conjunto de dados iris que vocês já utilizaram, foi coletado por Edgar Anderson e ficou famoso pelo trabalho de Ronald E. Fisher. Vamos carregar os dados no R.

data("iris")

###Após carregar o conjunto de dados, use o comando ?iris para entender mais sobre o conjunto de dados. Vamos então começar com as inspeções básicas do arquivo.

head(iris)
summary(iris)

## Conhecendo as funções aggregate e tapply
### Quantas informações por espécie?

table(iris$Species)

###Qual a média das variáveis por espécie? Vamos usar as funções agreggate e tapply. As duas funções são semelhantes, o que muda são os argumentos e o formato de saída de cada uma delas.

### media do comprimento de sepala por especie
?tapply
tapply(X = iris$Sepal.Length, INDEX = list(iris$Species), FUN = mean)

### a mesma tarefa, executada por outra funcao. Outros argumentos e outra saída
?aggregate
aggregate(x = iris$Sepal.Length, by = list(iris$Species), FUN = mean)

### ainda a mesma tarefa, com a mesma função mas em uma notação diferente
aggregate(Sepal.Length ~ Species, data = iris, mean)

### Podemos fazer o mesmo para as outras variáveis.

aggregate(Sepal.Length ~ Species, data = iris, mean)
aggregate(Sepal.Width ~ Species, data = iris, mean)
aggregate(Petal.Length ~ Species, data = iris, mean)

##E agora vamos calcular o desvio padrão das variáveis.
tapply(X = iris$Sepal.Length, INDEX = list(iris$Species), FUN = sd)
tapply(X = iris$Sepal.Width, INDEX = list(iris$Species), FUN = sd)
tapply(X = iris$Petal.Length, INDEX = list(iris$Species), FUN = sd)
tapply(X = iris$Petal.Width, INDEX = list(iris$Species), FUN = sd)

#Veja abaixo uma solução de como calular a média por espécie de todas as variáveis. Para isso, vamos usar o comando for e executar todas as tarefas em um mesmo ciclo.

## criando matriz de 3 colunas - uma para cada sp - e 4 linhas - uma para cada metrica
?matrix
medias <- matrix(NA, ncol = 3, nrow = 4)
medias

### definindo o nome das colunas e das linhas da matriz
?colnames
?unique
colnames(medias) <- unique(iris$Species)
medias #agora com os nomes das colunas

?names
rownames(medias) <- names(iris)[-5] #o -5 significa para retirar a quinta coluna
medias

for (i in 1:4){
    medias[i,] <- tapply(iris[,i], iris$Species, mean)
}

medias

# Estatísticas descritivas
## Medidas de tendência central
###Média

vars <- iris[, -5]
apply(vars, 2, mean)
?apply

## Mediana: 50º quantil, de forma que divide os dados em duas metades

apply(vars, 2, median)

## Moda: valor mais frequente na amostra
?sort
freq_sl <- sort(table(iris$Sepal.Length), decreasing = TRUE)
freq_sl

## para mostrar o valor da primeira linha, que neste caso, é o de maior frequência

freq_sl[1]

## Medidas de dispersão
### Variância: desvio da média

apply(vars, 2, var)

## Desvio padrão: raiz quadrada da variância
sd01 <- apply(vars, 2, sd)

## ou
apply(vars, 2, sd)

## outra forma:
sd02 <- apply(vars, 2, function(x) sqrt(var(x)))
sd01
sd02
sd01 == sd02

## Coeficiente de variação: medida relativa de desvio padrão
### Não existe no R base uma função para calcular o coeficiente de variação. Isto não é um problema. Vamos formalmente criar nossa primeira função de R. Para isso, usamos a função function

cv <- function(x){
    sd(x)/mean(x)*100
}
apply(vars, 2, cv)

## Quantis ou percentis
### or padrão, a função quantile retorna o mínimo, o 25º percentil, a mediana, o 50º percentil, o 75º percentil e o máximo, também conhecidos pelo sumário de cinco números proposto por Tuckey (que também é o retorno da função summary de um vetor numérico)
### É possível modificar os percentis desejados com o argumento probs

### sumario de 5 numeros
apply(vars, 2, quantile)

### 5%, 50% e 95%
apply(vars, 2, quantile, probs=c(0.05, 0.5, 0.95))

## Intervalo (range)
### O intervalo é a diferença entre o maior e o menor valor de determinada variável
### a funcao range nos retorna os valores minimo e maximo
apply(vars, 2, range)

### aplicando a funcao diff ao resultado do range, temos o valor desejado
### uma boa pratica é nunca sobrescrever um objeto já existente no R, por isso
### nunca nomeie um objeto com um nome já existente

my_range <- function(x){
    diff(range(x))
}
apply(vars, 2, my_range)

## Intervalo interquartil (IIQ)
### O IIQ é a diferença entre o quartil superior (75%) e o quartil inferior (25%).
apply(vars, 2, IQR)

## Correlação
cor(vars)





