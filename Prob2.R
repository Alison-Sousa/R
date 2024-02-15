# Função para simular o lançamento de um dado justo e calcular a probabilidade de obter um número específico
calculate_probability <- function(n_lancamentos, numero_alvo) {
  # Lançamento de um dado justo
  dados <- sample(1:6, n_lancamentos, replace = TRUE)
  
  # Contagem de quantas vezes o número alvo aparece
  ocorrencias_alvo <- sum(dados == numero_alvo)
  
  # Cálculo da probabilidade
  probabilidade <- ocorrencias_alvo / n_lancamentos
  
  return(probabilidade)
}

# Exemplo de uso da função
n_lancamentos <- 1000  # Número de lançamentos do dado
numero_alvo <- 4  # Número alvo para o qual queremos calcular a probabilidade
probabilidade <- calculate_probability(n_lancamentos, numero_alvo)

# Imprimir resultado
cat("A probabilidade de obter o número", numero_alvo, "em", n_lancamentos, "lançamentos de um dado justo é:", probabilidade, "\n")
