# Função para análise de sentimentos
analyze_sentiment <- function(texts) {
  # Dicionário de palavras e suas polaridades
  sentiment_dict <- list(
    "feliz" = 1,
    "triste" = -1,
    "bom" = 1,
    "ruim" = -1,
    "ótimo" = 1,
    "horrível" = -1
  )
  
  # Inicialização do vetor de pontuações
  scores <- numeric(length(texts))
  
  # Iteração sobre cada texto
  for (i in seq_along(texts)) {
    text <- tolower(texts[i])  # Converter texto para minúsculas
    words <- strsplit(text, "\\W+")[[1]]  # Dividir o texto em palavras
    
    # Calcular pontuação do texto
    score <- 0
    for (word in words) {
      if (word %in% names(sentiment_dict)) {
        score <- score + sentiment_dict[[word]]
      }
    }
    scores[i] <- score
  }
  
  return(scores)
}

# Exemplo de uso da função
textos <- c("Eu estou muito feliz hoje!", "Que dia ruim...")
pontuacoes <- analyze_sentiment(textos)
print(pontuacoes)

