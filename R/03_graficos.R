##############################################################
####Tutorial de Análise Exploratória de Dados GRÁFICOS   #####
####Sara Mortara, Andrea Sánchez-Tapia & Diogo S. B. Rocha###
#### fev 2020                                           #####

# Vamos carregar os dados IRIS no R.

data("iris")

# Métodos gráficos
## um pacote bastante poderoso para desenhar gráficos no R é o ggplot2. A sintaxe do ggplot2 é ligeiramente diferente do R base. Se achar muito difícil, não se preocupe. Todas as soluções podem ser feitas no R e com o tempo você se acostuma com o ggplot2

## Gráfico de barras
barplot(table(iris$Species))

## Histograma
### Vamos ver um exemplo de histograma padrão para os dados das espécies de Iris.

?par

par(mfrow=c(2, 2))
hist(iris$Sepal.Length)
hist(iris$Sepal.Width)
hist(iris$Petal.Length)
hist(iris$Petal.Length)

par(mfrow=c(1, 1)) #para tornar a janela da figura como única e não de 4. Deve-se usá-la após ter configurado uma janela dividida

## Agora para o comprimento da sépala das espécies de Iris, vamos ver o efeito do número de intervalos no histograma com o argumento breaks

par(mfrow=c(1, 2))
hist(iris$Sepal.Width)
hist(iris$Sepal.Width, breaks = 4)

par(mfrow=c(1, 2)) #para tornar a janela da figura como única e não de 2.

## Curva de densidade
###  Em comparação ao histograma, no eixo y, ao invés de termos a frequência, temos a densidade probabilística.

par(mfrow=c(1, 2))
hist(iris$Sepal.Width)
hist(iris$Sepal.Width, freq = FALSE)

par(mfrow=c(1, 1))

### No R podemos ver a curva de densidade a usando a função por meio do plot da função density
par(mfrow=c(1, 2))

### plot da curva de densidade
plot(density(iris$Sepal.Width))

### plot da curva de densidade sobre o histograma de densidade
hist(iris$Sepal.Width, freq = FALSE)
lines(density(iris$Sepal.Width), col="blue") # note que agora estamos usando a funcao o comando add=TRUE

par(mfrow=c(1, 1))

# Box-plot ou box-whisker plot
## Vamos agora fazer os box-plots das variáveis contidas no objeto iris. Vamos começar com as variáveis gerais.
boxplot(iris$Sepal.Length)
boxplot(iris$Sepal.Width)
boxplot(iris$Petal.Length)
boxplot(iris$Petal.Width)

## Agora vamos olhar para os valores por espécie.

boxplot(Sepal.Length ~ Species, data = iris)
boxplot(Sepal.Width ~ Species, data = iris)
boxplot(Petal.Length ~ Species, data = iris)
boxplot(Petal.Width ~ Species, data = iris)

## Vamos usar a própria função boxplot para identificar os outliers.

boxplot(iris$Sepal.Width)

my_boxplot <- boxplot(iris$Sepal.Width, plot=FALSE)
my_boxplot

### o objeto é uma lista e os valores outliers estão guardados no elemento $out da lista
outliers <- my_boxplot$out

### qual a posicao dos outliers
which(iris$Sepal.Width %in% outliers)

### vamos usar a posicao para indexar o objeto
iris[which(iris$Sepal.Width %in% outliers), c("Sepal.Width", "Species")]

## No caso anterior consideramos outliers em relação à distribuição da variável para todas as espécies juntas. É razoável assumir que cada espécie tenha um padrão morfométrico distinto de modo que poderíamos identificar outliers de maneira espécie específica.

boxplot(Sepal.Width ~ Species, data = iris)

my_boxplot2 <- boxplot(Sepal.Width ~ Species, data=iris, plot=FALSE)
my_boxplot2 # o objeto é uma lista e os valores outliers estão guardados no elemento $out da lista

outliers2 <- my_boxplot2$out

### neste caso, queremos apenas os outliers da especie setosa
### vamos usar a posicao para indexar o objeto
iris[iris$Sepal.Width %in% outliers2 &
         iris$Species == "setosa",
     c("Sepal.Width", "Species")]

# Entendendo a distribuição dos dados
## Vamos olhar para os dados morfométricos das espécies de Iris e comparar com uma distribuição normal. No R, isto pode ser feito de forma visual com as funções qqnorm e qqline.

par(mfrow = c(1,3))
qqnorm(iris$Sepal.Length[iris$Species == "setosa"],
       main = "setosa")
qqline(iris$Sepal.Length[iris$Species == "setosa"])
qqnorm(iris$Sepal.Length[iris$Species == "versicolor"],
       main = "versicolor")
qqline(iris$Sepal.Length[iris$Species == "versicolor"])
qqnorm(iris$Sepal.Length[iris$Species == "virginica"],
       main = "virginica")
qqline(iris$Sepal.Length[iris$Species == "virginica"])

par(mfrow=c(1,1))

#-------------------------------------------------------------------------------

# Relação entre variáveis
## Uma função no R que nos ajuda a explorar a relação entre muitas variáveis é a pairs. O resultado é uma matriz com variáveis em linhas e colunas o gráfico que vemos é o gráfico de dispersão para cada par de variáveis. A diagonal da matriz contém os nomes das variáveis. Note que o gráfico é espelhado de modo que a relação entre tamanho e comprimento de sépala aparece tanto na linha 1 e coluna 2 como na linha 2 e coluna 1

vars <- iris[, -5]

pairs(vars)

#---------------------------------------------------------------------------------
# EXTRA!
## O pacote de R GGally fornece uma saída muito interessante para avaliar as relações entre variáveis pois já mostra o valor de correlação entre variáveis e a curva de densidade probabilística de cada uma das variáveis.

# EXEPCIONALMENTE vamos carregar o pacote agora, já que esse é um exercício bonus.
library("GGally")
ggpairs(vars)
