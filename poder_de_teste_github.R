# PODER DE TESTE ----------------------------------------------------------

library(Rlab)

gerar_amostra <- function(p,n,shape,rate){
  amostra <- NULL
  for (i in 1:n){
    valor <- rbern(1,1-p) #prob de 1-p de obter valor 1, e prob p de obter valor 0 
    if (valor == 1){
      amostra[i] <- rgamma(1,shape,rate)
    }
    else amostra[i] <- valor
  }
  return(amostra)
}

# teste de Welch ----------------------------------------------------------

valorp_teste_t <- function(x,y){
  valorp <- t.test(x,y,alternative='two.sided')$p.value 
  return(valorp)
}


# teste bootstrap 16.1 -----------------------------------------------------

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


# poder de teste ----------------------------------------------------------
poder_de_teste <- function(p,n,shape1,rate1,shape2,rate2,B,amostra,alpha1,alpha2,alpha3){
  vetor_t_test <- c()
  vetor_boost_1 <- c()
  vetor_boost_2 <- c()
  vetor_boost_3 <- c()
  
  # com variâncias diferentes -----------------------------------------------
  
  
  # rejeitar H0 dado que H0 é falsa (verdadeiro positivo)
  for (i in 1:amostra){
    x <- gerar_amostra(p,n,shape1,rate1) #sob H1
    y <- gerar_amostra(p,n,shape2,rate2) #sob H1
    vetor_t_test[i] <- valorp_teste_t(x,y)
    vetor_boost_1[i] <- bootstrap_1(x,y,B)
    vetor_boost_2[i] <- bootstrap_2(x,y,B)
    vetor_boost_3[i] <- bootstrap_3(x,y,B)
  }
  entradas_matrizes <- c()
  alpha <- c(alpha1,alpha2,alpha3)
  for (i in 1:length(alpha)){
    poder1 <- sum(vetor_t_test < alpha[i])/amostra
    entradas_matrizes <- c(entradas_matrizes,poder1)
    
    poder2 <- sum(vetor_boost_1 < alpha[i])/amostra
    entradas_matrizes <- c(entradas_matrizes,poder2)
    
    poder3 <- sum(vetor_boost_2 < alpha[i])/amostra
    entradas_matrizes <- c(entradas_matrizes,poder3)
    
    poder4 <- sum(vetor_boost_3 < alpha[i])/amostra
    entradas_matrizes <- c(entradas_matrizes,poder4)
  }    
  poder_teste <- matrix(entradas_matrizes,nrow=3,ncol=4,byrow=T)
  return(poder_teste)
  
}


set.seed(1)
semente <- sample(1:1000,1,replace=T)
set.seed(semente)


# CENÁRIO 1- AMOSTRA 1 ---------------------------------------------------------------


set.seed(semente)
poder_2_1 <- poder_de_teste(p=0.2,n=10,shape1=4,rate1=4,shape2=9,rate2=6,B=1000,amostra=5000,alpha1=0.01,alpha2=0.05,alpha3=0.1)
poder_2_1

# código caso desbalanceado -----------------------------------------------

# poder de teste ----------------------------------------------------------
poder_de_teste <- function(p,n1,n2,shape1,rate1,shape2,rate2,B,amostra,alpha1,alpha2,alpha3){
  vetor_t_test <- c()
  vetor_boost_1 <- c()
  vetor_boost_2 <- c()
  vetor_boost_3 <- c()
  
  # rejeitar H0 dado que H0 é falsa (verdadeiro positivo)
  for (i in 1:amostra){
    x <- gerar_amostra(p,n1,shape1,rate1) #sob H1
    y <- gerar_amostra(p,n2,shape2,rate2) #sob H1
    vetor_t_test[i] <- valorp_teste_t(x,y)
    vetor_boost_1[i] <- bootstrap_1(x,y,B)
    vetor_boost_2[i] <- bootstrap_2(x,y,B)
    vetor_boost_3[i] <- bootstrap_3(x,y,B)
  }
  entradas_matrizes <- c()
  alpha <- c(alpha1,alpha2,alpha3)
  for (i in 1:length(alpha)){
    poder1 <- sum(vetor_t_test < alpha[i])/amostra
    entradas_matrizes <- c(entradas_matrizes,poder1)
    
    poder2 <- sum(vetor_boost_1 < alpha[i])/amostra
    entradas_matrizes <- c(entradas_matrizes,poder2)
    
    poder3 <- sum(vetor_boost_2 < alpha[i])/amostra
    entradas_matrizes <- c(entradas_matrizes,poder3)
    
    poder4 <- sum(vetor_boost_3 < alpha[i])/amostra
    entradas_matrizes <- c(entradas_matrizes,poder4)
  }    
  poder_teste <- matrix(entradas_matrizes,nrow=3,ncol=4,byrow=T)
  return(poder_teste)
  
}




# CENÁRIO 4- AMOSTRA 1 ---------------------------------------------------------------


set.seed(semente)
poder4_1_1 <- poder_de_teste(p=0.2,n1=10,n2=20,shape1=4,rate1=4,shape2=9,rate2=6,B=1000,amostra=5000,alpha1=0.01,alpha2=0.05,alpha3=0.1)
poder4_1_1

# com variâncias diferentes -----------------------------------------------

# poder de teste ----------------------------------------------------------
poder_de_teste <- function(p,n,shape1,rate1,shape2,rate2,B,amostra,alpha1,alpha2,alpha3){
  vetor_t_test <- c()
  vetor_boost_1 <- c()
  vetor_boost_2 <- c()
  vetor_boost_3 <- c()
  
  # rejeitar H0 dado que H0 é falsa (verdadeiro positivo)
  for (i in 1:amostra){
    x <- gerar_amostra(p,n,shape1,rate1) #sob H1
    y <- gerar_amostra(p,n,shape2,rate2) #sob H1
    vetor_t_test[i] <- valorp_teste_t(x,y)
    vetor_boost_1[i] <- bootstrap_1(x,y,B)
    vetor_boost_2[i] <- bootstrap_2(x,y,B)
    vetor_boost_3[i] <- bootstrap_3(x,y,B)
  }
  entradas_matrizes <- c()
  alpha <- c(alpha1,alpha2,alpha3)
  for (i in 1:length(alpha)){
    poder1 <- sum(vetor_t_test < alpha[i])/amostra
    entradas_matrizes <- c(entradas_matrizes,poder1)
    
    poder2 <- sum(vetor_boost_1 < alpha[i])/amostra
    entradas_matrizes <- c(entradas_matrizes,poder2)
    
    poder3 <- sum(vetor_boost_2 < alpha[i])/amostra
    entradas_matrizes <- c(entradas_matrizes,poder3)
    
    poder4 <- sum(vetor_boost_3 < alpha[i])/amostra
    entradas_matrizes <- c(entradas_matrizes,poder4)
  }    
  poder_teste <- matrix(entradas_matrizes,nrow=3,ncol=4,byrow=T)
  return(poder_teste)
  
}


set.seed(semente)
podervar_1_1 <- poder_de_teste(p=0.2,n=10,shape1=4,rate1=4,shape2=2.25,rate2=1.5,B=1000,amostra=5000,alpha1=0.01,alpha2=0.05,alpha3=0.1)
podervar_1_1