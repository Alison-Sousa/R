simulate_store_inventory <- function(estoque_inicial, vendas, reposicoes) {
  # Simular mudanças no estoque ao longo do tempo
  for (dia in seq_along(vendas)) {
    estoque_inicial <- estoque_inicial - vendas[dia] + reposicoes[dia]
    
    # Garantir que o estoque não fique negativo
    if (estoque_inicial < 0) {
      estoque_inicial <- 0
    }
  }
  
  return(estoque_inicial)
}

# Exemplo de uso da função
estoque_inicial <- 100
vendas <- c(10, 15, 20, 25, 30)  # Vendas diárias
reposicoes <- c(20, 15, 10, 5, 0)  # Reposições diárias
periodo <- length(vendas)
estoque_final <- simulate_store_inventory(estoque_inicial, vendas, reposicoes)
cat("Estoque final após", periodo, "dias:", estoque_final, "\n")
