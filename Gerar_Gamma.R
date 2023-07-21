library(ggplot2)

# Definir os parâmetros da distribuição gama
shape <- 4     # Parâmetro de forma (alpha)
rate <-  4   # Parâmetro de taxa (beta)

# Gerar uma sequência de valores para o eixo x
x <- seq(0, 8, by = 0.1)

# Calcular a densidade da distribuição gama para a sequência de valores x
density <- dgamma(x, shape = shape, rate = rate)

# Criar um dataframe para o gráfico
data <- data.frame(x = x, density = density)


ggplot(data, aes(x=x,y=density))+ geom_line(color='blue') + 
  labs(title = "Gráfico de Densidade da Distribuição Gama(4,4)",
          x = "Valores de x",
          y = "Densidade") +
          theme_minimal() + theme(plot.title = element_text(hjust = 0.5))
