# Carregar biblioteca necessária para calcular o Índice de Moran
library(spdep)

# Definir dados fictícios de temperatura média mensal em diferentes cidades
cidades <- c("Cidade A", "Cidade B", "Cidade C", "Cidade D")
temperatura <- c(25, 22, 27, 23)  # Temperatura média em graus Celsius
meses <- c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho", "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro")
dados <- data.frame(Cidade = rep(cidades, each = 12),
                    Mes = rep(meses, times = length(cidades)),
                    Temperatura = rep(temperatura, times = 12))

# Calcular a matriz de pesos espaciais (podemos usar uma matriz de adjacência simples neste exemplo)
# Suponha que as cidades são adjacentes na ordem em que estão na lista 'cidades'
matriz_adjacencia <- matrix(0, nrow = length(cidades), ncol = length(cidades))
diag(matriz_adjacencia) <- 0  # Não considerar a diagonal principal
for (i in 1:(length(cidades) - 1)) {
  matriz_adjacencia[i, (i + 1)] <- 1
  matriz_adjacencia[(i + 1), i] <- 1
}

# Calcular o Índice de Moran
indice_moran <- moran.test(dados$Temperatura, listw = matriz_adjacencia)
print(indice_moran)

