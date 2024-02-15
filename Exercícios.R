# Função para simular um jogo de cartas e calcular a probabilidade de um jogador receber uma carta específica
simulate_card_game <- function(numero_jogadores, carta_alvo) {
  # Gerar um baralho fictício (52 cartas)
  baralho <- rep(c("Ás", "Dois", "Três", "Quatro", "Cinco", "Seis", "Sete", "Oito", "Nove", "Dez", "Valete", "Dama", "Rei"), each = 4)
  naipes <- rep(c("Espadas", "Paus", "Copas", "Ouros"), times = 13)
  baralho <- paste(baralho, "de", naipes)
  
  # Distribuir cartas aleatórias para os jogadores
  cartas_jogadores <- lapply(1:numero_jogadores, function(x) sample(baralho, size = 5))
  
  # Calcular a probabilidade de um jogador receber uma carta específica
  probabilidade_carta_alvo <- mean(sapply(cartas_jogadores, function(x) any(x == carta_alvo)))
  
  # Retornar as cartas de cada jogador e a probabilidade do jogador alvo receber a carta alvo
  return(list(cartas_jogadores = cartas_jogadores, probabilidade_carta_alvo = probabilidade_carta_alvo))
}

# Exemplo de uso da função
resultado_jogo <- simulate_card_game(numero_jogadores = 4, carta_alvo = "Ás de Espadas")

# Imprimir as cartas de cada jogador
cat("Cartas de cada jogador:\n")
for (i in seq_along(resultado_jogo$cartas_jogadores)) {
  cat("Jogador", i, ": ", paste(resultado_jogo$cartas_jogadores[[i]], collapse = ", "), "\n")
}

# Imprimir a probabilidade do jogador alvo receber a carta alvo
cat("\nProbabilidade do jogador receber a carta alvo:", resultado_jogo$probabilidade_carta_alvo, "\n")

simulate_sports_tournament <- function(equipes, rodadas) {
  # Inicializar tabela de classificação
  tabela_classificacao <- data.frame(Equipe = equipes, Pontos = rep(0, length(equipes)))
  
  # Simular rodadas do torneio
  for (rodada in 1:rodadas) {
    # Simular resultados dos jogos (pontuação aleatória)
    resultados <- sample(0:3, length(equipes) * (length(equipes) - 1) / 2, replace = TRUE)
    resultados <- matrix(resultados, ncol = length(equipes) - 1, byrow = TRUE)
    
    # Atualizar tabela de classificação com os resultados da rodada
    for (jogo in 1:(length(equipes) - 1)) {
      tabela_classificacao[resultados[jogo, 1] + 1, "Pontos"] <- tabela_classificacao[resultados[jogo, 1] + 1, "Pontos"] + resultados[jogo, 2]
      tabela_classificacao[resultados[jogo, 2] + 1, "Pontos"] <- tabela_classificacao[resultados[jogo, 2] + 1, "Pontos"] + resultados[jogo, 1]
    }
  }
  
  # Ordenar tabela de classificação por pontos
  tabela_classificacao <- tabela_classificacao[order(-tabela_classificacao$Pontos), ]
  
  return(tabela_classificacao)
}

# Exemplo de uso da função
equipes <- c("Time A", "Time B", "Time C", "Time D")
rodadas <- 5
classificacao_final <- simulate_sports_tournament(equipes, rodadas)
print("Classificação final do torneio:")
print(classificacao_final)

