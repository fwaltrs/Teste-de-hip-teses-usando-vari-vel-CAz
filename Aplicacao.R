
# APLICAÇÃO ---------------------------------------------------------------
library(readxl)
Healthy_controls <- read_excel("C:/Users/ferna/Dropbox/PC/Downloads/grupo_controle.xlsx")
T2DM <- read_excel("C:/Users/ferna/Dropbox/PC/Downloads/T2DM.xlsx")

x <- Healthy_controls$`Basophil count`
y <- T2DM$`Basophil count`
mean(x)
mean(y)

# Análise descritiva de cada variável -------------------------------------

summary(x)
## boxplot e percentual de zeros
categoria <- rep(c("Controle", "T2DM"), each = 120)

# Organizar os dados em um data frame
dados <- data.frame(Grupos = categoria, Valor = c(x, y))
library(ggplot2)
# Criar o boxplot lado a lado
ggplot(dados, aes(x = Grupos, y = Valor, fill = Grupos)) +
  geom_boxplot() +
  labs(title = "Boxplot dos grupos",
       x = "Grupos",
       y = "Basophil count") +
  theme(plot.title = element_text(hjust = 0.5))

#porcentagem de valor 0 em cada grupo
#grupo controle
length(which(x==0))/120*100
length(which(y==0))/120*100


## testes
valorp_teste_t <- function(x,y){
  valorp <- t.test(x,y,alternative='two.sided')$p.value 
  return(valorp)
}
set.seed(29)
teste_t <- valorp_teste_t(x,y)

# teste bootstrap 16.1 -----------------------------------------------------
B <- 1000
bootstrap_1 <- function(x,y,B){
  amostra_combinada <- c(x,y)
  estat_boos1 <- abs(mean(x) - mean(y))
  boos1 <- c()
  for (i in 1:B){
    obs1 <- sample(seq(1,length(amostra_combinada),1),length(amostra_combinada),replace=TRUE)
    amostra <- amostra_combinada[obs1]
    valor <- mean(amostra[1:length(x)]) - mean(amostra[(length(x)+1):length(amostra_combinada)])
    boos1[i] <- abs(valor)
  }
  
  valor_p <- sum(1*(boos1 > estat_boos1))/B
  return(valor_p)
}
set.seed(29)
boots1 <- bootstrap_1(x,y,B)
# Algoritmo 16.2 ----------------------------------------------------------

bootstrap_2 <- function(x,y,B){
  z <- c(x,y)
  xtrans <- x - mean(x) + mean(z)
  ytrans <- y - mean(y) + mean(z)
  #transformando sob h0
  
  est_obs <- (mean(x) - mean(y))/(sqrt(var(x)/length(x) + var(y)/length(y)))
  est_obs <- abs(est_obs)
  estat_2 <- c()
  for (i in 1:B){
    amostra_x<-sample(seq(1,length(xtrans),1),length(xtrans),replace=TRUE)
    amostra_y<-sample(seq(1,length(ytrans),1),length(ytrans),replace=TRUE)
    valor <- (mean(xtrans[amostra_x]) - mean(ytrans[amostra_y]))/(sqrt(var(xtrans[amostra_x])/length(xtrans[amostra_x]) + var(ytrans[amostra_y])/length(ytrans[amostra_y])))
    estat_2[i] <- abs(valor)
  }
  
  valorp <- sum(estat_2 > est_obs)/B
  return(valorp)
}
set.seed(29)
boots2 <- bootstrap_2(x,y,B)

# Algoritmo Dwivedi -------------------------------------------------------

bootstrap_3 <- function(x,y,B){
  n <- length(x)
  m <- length(y)
  est_obs <- (mean(x) - mean(y))/(sqrt(var(x)/n + var(y)/m))
  est_obs <- abs(est_obs)
  x_trans <- x - mean(x)
  y_trans <- y - mean(y)
  
  estat_3 <- c()
  for (i in 1:B){
    amos_x <-sample(seq(1,length(x_trans),1),length(x_trans),replace=TRUE)
    amos_y <-sample(seq(1,length(y_trans),1),length(y_trans),replace=TRUE)
    valor <- (mean(x_trans[amos_x]) - mean(y_trans[amos_y]))/(sqrt(var(x_trans[amos_x])/length(x_trans[amos_x]) + var(y_trans[amos_y])/length(y_trans[amos_y])))
    valor <- abs(valor)
    estat_3[i] <- valor
  }
  
  valorp <- sum(estat_3 > est_obs)/B
  return(valorp)
  
}
set.seed(29)
boots3 <- bootstrap_3(x,y,B)

tabela <- cbind(teste_t,boots1,boots2,boots3)
tabela




# SORTEIO DE 20 PACIENTES DE CADA GRUPO -----------------------------------

set.seed(29)
sorteio1 = sample(seq(1,120,1),20,replace=FALSE)
x1 = x[sorteio1]

set.seed(95)
sorteio2 = sample(seq(1,120,1),20,replace=FALSE)
y1 = y[sorteio2]

#porcentagem de valor 0 em cada grupo
#grupo controle
length(which(x1==0))/20*100
length(which(y1==0))/20*100

# boxplot
categoria <- rep(c("Controle", "T2DM"), each = 20)
dados <- data.frame(Grupos = categoria, Valor = c(x1, y1))
library(ggplot2)

ggplot(dados, aes(x = Grupos, y = Valor, fill = Grupos)) +
  geom_boxplot() +
  labs(title = "Boxplot dos grupos",
       x = "Grupos",
       y = "Basophil count") +
  theme(plot.title = element_text(hjust = 0.5))


boots3 <- bootstrap_3(x1,y1,B)

tabela <- cbind(teste_t,boots1,boots2,boots3)
tabela
mean(x1)
mean(y1)

### criar uma função para sortear amostras 
n1 <- 20 
amostra_x1 <- function(n1,x){
  set.seed(29)
  sorteio1 = sample(seq(1,120,1),n1,replace=FALSE)
  x1 = x[sorteio1]
  return (x1)}

amostra_y1 <- function(n1,y){
  set.seed(95)
  sorteio2 = sample(seq(1,120,1),n1,replace=FALSE)
  y1 = y[sorteio2]
  return (y1)}

entradas <- c()
while (n1 < 101){
  x1 <- amostra_x1(n1,x)
  y1 <- amostra_y1(n1,y)
  entradas <- c(entradas, valorp_teste_t(x1,y1))
  entradas <- c(entradas,bootstrap_1(x1,y1,B))
  entradas <- c(entradas,bootstrap_2(x1,y1,B))
  entradas <- c(entradas,bootstrap_3(x1,y1,B))
  n1 = n1 + 5
}

matriz_resultados <- matrix(entradas, ncol=4,byrow=TRUE)
nomes_colunas <- c("Teste T", "Bootstrap 1","Bootstrap 2","Bootstrap 3")
nomes_linhas <- c("20","25","30","35","40","45","50","55","60","65","70","75","80","85","90","95","100")
colnames(matriz_resultados) <- nomes_colunas
rownames(matriz_resultados) <- nomes_linhas

matriz_resultados

