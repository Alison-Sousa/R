stairs <- function(n, k) {
  # Caso base: Exatamente no topo
  if (n == 0) {
    return(1)
  }
  
  # Caso base: Menos de 0 degraus (impossível)
  if (n < 0) {
    return(0)
  }
  
  # Caso recursivo: Soma as maneiras para cada tamanho de passo
  ways <- 0
  for (step in 1:min(k, n)) {
    ways <- ways + stairs(n - step, k)
  }
  
  return(ways)
}

# Testando a função com n = 4 degraus e k = 3 passos máximos
cat("Número de maneiras de subir uma escada de 4 degraus (1, 2 ou 3 degraus por vez): ", stairs(4, 3), "\n")

