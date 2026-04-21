install.packages(c("cluster", "igraph"))
library(cluster)
library(igraph)

dados_bin <- dados[,-c(5,13,14)]

dados_bin[] <- lapply(dados_bin, factor)

dist_gower <- daisy(dados_bin, metric = "gower")

sim <- 1 - as.matrix(dist_gower)

threshold <- 0.7
adj <- sim > threshold

library(igraph)
g <- graph_from_adjacency_matrix(sim, 
                                 mode = "undirected", 
                                 weighted = TRUE, 
                                 diag = FALSE)

louvain <- cluster_louvain(g, weights = E(g)$weight)
modularity(louvain)

  louvain <- cluster_louvain(g)
clusters <- membership(louvain)

table(louvain = clusters, lca = dados$classe)

sim_filtrada <- sim
sim_filtrada[sim_filtrada < 0.5] <- 0
