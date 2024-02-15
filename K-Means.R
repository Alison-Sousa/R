# Carregar bibliotecas necessárias
library(ggplot2)
library(factoextra)

# Gerar dados fictícios de pontos de interesse em um mapa
set.seed(42)
n_pontos <- 1000
dados <- data.frame(X = rnorm(n_pontos, mean = 0, sd = 10),
                    Y = rnorm(n_pontos, mean = 0, sd = 10))

# Visualizar os dados no mapa
ggplot(dados, aes(x = X, y = Y)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Distribuição Espacial de Pontos de Interesse")

# Calcular o número ótimo de clusters usando o método Elbow
wss <- numeric(10)
for (i in 1:10) {
  kmeans_result <- kmeans(dados, centers = i)
  wss[i] <- sum(kmeans_result$withinss)
}
numero_clusters <- fviz_nbclust(dados, kmeans, method = "wss")$ch.elbow

# Realizar a clusterização usando K-means com o número ótimo de clusters
kmeans_result <- kmeans(dados, centers = numero_clusters)

# Visualizar os clusters no mapa
ggplot(dados, aes(x = X, y = Y, color = factor(kmeans_result$cluster))) +
  geom_point() +
  theme_minimal() +
  labs(title = "Clusterização de Pontos de Interesse")

# Calcular o Índice de Silhueta para avaliar a qualidade da clusterização
silhouette <- silhouette(kmeans_result$cluster, dist(dados))
mean_silhouette <- mean(silhouette[, "sil_width"])
cat("Índice de Silhueta Médio:", mean_silhouette, "\n")
