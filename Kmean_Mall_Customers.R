# -------------------------------------------------------------------------
#                    SEGMENTAÇÃO DE CLIENTES
#                      ANÁLISE DE CLUSTER 
# -------------------------------------------------------------------------
# Nome do Dataset: Mall Customer Segmentation Data
# Fonte: https://www.kaggle.com/datasets/vjchoudhary7/customer-segmentation-tutorial-in-python?datasetId=42674&language=R
# -------------------------------------------------------------------------

# Leitura das Bibliotecas -------------------------------------------------
install.packages("readr")
library(tidyverse)  # CRAN 1.3.2 - Data manipulation
library(cluster)    # CRAN 2.1.3 - Algoritmo de clusterização
library(factoextra) # CRAN 1.0.7 - Vizualização do Algoritmo de Clusterização
library(gridExtra)  # CRAN 2.3   - Biblioteca gráfica
library(ggthemes)   # CRAN 4.2.4 - Biblioteca de temas gráficos
library(ggfortify)  # CRAN 0.4.14 - Vizualização de dados estatístico
library(readr)      # CRAN 2.1.2 - Leitura de dados em texto
library(ggplot2)    # CRAN 3.3.6 - Vizualização de gráficos elegantes


# Leitura do Dataset ------------------------------------------------------
dataset <- read.csv('Mall_Customers.csv')

# Verificando NAs
colSums(is.na(dataset))
anyNA(dataset)
apply(is.na(dataset), 2, sum)

# Verificando a estrutura do Dataset
glimpse(dataset)

# Renomeando as colunas
names(dataset)
dataset <- dataset |> 
  dplyr::rename(renda = "Annual.Income..k..",
                score = "Spending.Score..1.100.")


dataset_df <- scale(dataset[,c(4,5)]) # Padronização dos dados
class(dataset_df)
names(dataset)


# Tansformando em dataset as colunas normalizadas
dataset_df <- data.frame(dataset_df)



# Vizualzando o formato dos dados
head(dataset_df)
glimpse(dataset_df)


# Vizualização dos dados --------------------------------------------------

ggplot(dataset_df, aes(renda, score)) +
  geom_point(colour = "cyan4", size = 4) +
  geom_smooth(method = "lm") +
  ggtitle("Clientes") +
  xlab("Renda") +
  ylab("Score")
  
# Correlação entre as variáveis
cor(dataset_df$renda, dataset_df$score)

# Regressão linear
lm <- lm(renda~score, data = dataset_df)
summary(lm)



# Modelo Não Supervisionado Kmeans ----------------------------------------

k2 <- kmeans(dataset_df,  # Banco de dados padronizado
             centers = 2, # Quantidade de centroides
             nstart = 25  # Quantidades de configurações
             )

k3 <- kmeans(dataset_df,  # Banco de dados padronizado
             centers = 3, # Quantidade de centroides
             nstart = 25  # Quantidades de configurações
             )

k4 <- kmeans(dataset_df,  # Banco de dados padronizado
             centers = 4, # Quantidade de centroides
             nstart = 25  # Quantidades de configurações
             )

k5 <- kmeans(dataset_df,  # Banco de dados padronizado
             centers = 5, # Quantidade de centroides
             nstart = 25  # Quantidades de configurações
             )


# Vizualizando o resultado
g2 <- fviz_cluster(k2, data = dataset_df, ggtheme = theme_bw(), geom = "point") + 
  ggtitle("k=2")
g3 <- fviz_cluster(k3, data = dataset_df, ggtheme = theme_bw(), geom = "point") + 
  ggtitle("k=3")
g4 <- fviz_cluster(k4, data = dataset_df, ggtheme = theme_bw(), geom = "point") + 
  ggtitle("k=4")
g5 <- fviz_cluster(k5, data = dataset_df, ggtheme = theme_bw(), geom = "point") + 
  ggtitle("k=5")

# Vizualizar em escala
grid.arrange(g2, g3, g4, g5,
             nrow = 2) # Qtd de linhas


# Verificando o n ideial de Cluster Elbow Method --------------------------

set.seed(123)
wss <- function(k){
  kmeans(dataset_df, k, nstart = 10)$tot.withinss
}

k.valor <- 1:15


k.valores <- map_dbl(k.valor, wss)


plot(k.valor, k.valores,
     type = "b", pch = 19, frame = FALSE,
     xlab = "Número de Cluster 'k'",
     ylab = "Total de sqtr dos Cluster");grid()
abline(v = 5, lwd = 2, lty=3, col = "cyan4")

# O ideal será 5 cluster, pois é o ponto onde a curva dobra.


set.seed(123)

fviz_nbclust(dataset_df, kmeans, method = "wss")
fviz_nbclust(dataset_df, kmeans, method = "silhouette")




dataset$k2 <- k2$cluster
dataset$k3 <- k3$cluster
dataset$k4 <- k4$cluster
dataset$k5 <- k5$cluster


# Vizualziando o resultado
par(mfrow=c(2,2))
boxplot(Age~k2, dataset, main = "BOXPLOT = Idade vs k2")
boxplot(Age~k3, dataset, main = "BOXPLOT = Idade vs k3")
boxplot(Age~k4, dataset, main = "BOXPLOT = Idade vs k4")
boxplot(Age~k5, dataset, main = "BOXPLOT = Idade vs k5")

boxplot(renda~k2, dataset, main = "BOXPLOT = Idade vs k2")
boxplot(renda~k3, dataset, main = "BOXPLOT = Idade vs k3")
boxplot(renda~k4, dataset, main = "BOXPLOT = Idade vs k4")
boxplot(renda~k5, dataset, main = "BOXPLOT = Idade vs k5")


