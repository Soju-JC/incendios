# library(tidyverse)
# library(sf) # Objeto de coordenadas 
# library(dbscan) # Clusterização
# library(lubridate)
# library(leaflet) # Plot
# library(leaflet.extras) # Plot
# library(htmlwidgets) # Plot
# library(stats) # Inter-Intra-clusters
# library(data.table) # Load dataset
# 
# ################################################################################
# ##### Dataset para o tempo até o término da queimada/ e tempo até recidiva #####
# ################################################################################                         
# # Carregar e filtrar os dados
# # df_inpe_part1 <- fread("focos_br_todos-sats_2020.csv", header = TRUE)
# df_inpe_part2 <- fread("focos_br_todos-sats_2021.csv", header = TRUE)
# df_inpe_part3 <- fread("focos_br_todos-sats_2022.csv", header = TRUE)
# df_inpe_part4 <- fread("focos_br_todos-sats_2023.csv", header = TRUE)
# 
# # Combinar os datasets em um único dataframe
# df_inpe_all <- rbind(
#   # df_inpe_part1, 
#   df_inpe_part2,
#   df_inpe_part3, 
#   df_inpe_part4
#   )
# 
# df_inpe2 <- subset(df_inpe_all, df_inpe_all$estado == "ESPÍRITO SANTO")
# 
# # Converter para sf e transformar CRS para métrico
# fire_data <- st_as_sf(
#   df_inpe2, coords = c("longitude", "latitude"), crs = 4326
#   ) %>% 
#   st_transform(crs = 32633)
# 
# # Configurações de DBSCAN
# eps_spatial <- 1000   # Distância máxima espacial em metros
# eps_temporal <- 7 * 24   # 3 dias em horas 
# minPts <- 5 # Quantidade mínima de pontos necessários
# 
# # Clustering espacial e temporal sem limitar por município
# clustered_data <- fire_data %>%
#   mutate(
#     time_in_hours = as.numeric(
#       difftime(data_pas, min(data_pas), units = "hours")
#       ),
#     # É necessário dimensionar adequadamente a dimensão temporal para  
#     #torná-la compatível com as distâncias espaciais utilizadas no  
#     #algoritmo de agrupamento DBSCAN e captar melhor a realidade.
#     time_scaled = time_in_hours * (eps_spatial / eps_temporal)
#   ) %>%
#   group_modify(~ {
#     coords <- st_coordinates(.x)
#     combined_data <- cbind(coords, .x$time_scaled)
#     clusters <- dbscan(combined_data, eps = eps_spatial, minPts = minPts)
#     .x$cluster_estado <- clusters$cluster
#     return(.x)
#   }) %>%
#   ungroup() %>%
#   filter(cluster_estado > 0) %>% # Remover clusters de ruído (0)  
#   st_as_sf(crs = 32633) # Reatribuir CRS métrico          
# 
# # Adicionar informações temporais por cluster
# filtered_clusters <- clustered_data %>%
#   group_by(cluster_estado) %>%
#   mutate(
#     start_time = min(data_pas),
#     end_time = max(data_pas),
#     time_diff = as.numeric(difftime(end_time, start_time, units = "mins"))
#   ) %>%
#   filter(time_diff > 0) %>%  # Remover clusters inválidos
#   ungroup() %>%
#   mutate(duration = round(time_diff / 60, 2))  # Duração em horas
# 
# # Calcular Bounding Box e centroids para clusters estaduais
# cluster_data_summary <- filtered_clusters %>%
#   group_by(cluster_estado) %>%
#   summarize(
#     cluster_bbox = st_as_sfc(st_bbox(geometry)),
#     cluster_centroid = st_centroid(st_union(geometry))
#   ) %>%
#   ungroup() %>%
#   st_as_sf() %>%
#   st_transform(crs = 4326)  # Transformar para EPSG:4326
# 
# # centroides como um sf separado
# centroids <- cluster_data_summary %>%
#   st_drop_geometry() %>%
#   # Usar geometria existente
#   mutate(geometry = st_geometry(cluster_centroid)) %>%  
#   # Não redefinir CRS desnecessariamente
#   st_as_sf()  
# 
# # Renomear a coluna geometry para focus_geometry
# filtered_clusters <- filtered_clusters %>%
#   rename(focus_geometry = geometry)
# 
# # Adicionar geometrias de centroid e bounding box como variáveis separadas
# filtered_clusters <- filtered_clusters %>%
#   group_by(cluster_estado) %>%
#   mutate(
#     # Calcular geometria do centroide
#     centroid_geometry = st_centroid(st_union(focus_geometry)),
#     # Calcular geometria da bounding box
#     bbox_geometry = st_as_sfc(st_bbox(focus_geometry))          
#   ) %>%
#   ungroup()
# 
# # Verifica o CRS
# st_crs(filtered_clusters)
# 
# # Garantir transformação de filtered_clusters e centroids para EPSG:4326
# filtered_clusters <- filtered_clusters %>%
#   mutate(
#     focus_geometry = st_transform(focus_geometry, crs = 4326),
#     centroid_geometry = st_transform(centroid_geometry, crs = 4326),
#     bbox_geometry = st_transform(bbox_geometry, crs = 4326)
#   )
# 
# centroids <- centroids %>%
#   mutate(
#     geometry = st_transform(geometry, crs = 4326),
#     cluster_centroid = st_transform(cluster_centroid, crs = 4326)
#   )
# 
# cluster_data_summary <- cluster_data_summary %>%
#   mutate(
#     cluster_bbox = st_transform(cluster_bbox, crs = 4326),
#     cluster_centroid = st_transform(cluster_centroid, crs = 4326)
#   )
# 
# # Calcula distancias intra-cluster (compactação)
# cluster_compactness <- filtered_clusters %>%
#   group_by(cluster_estado) %>%
#   summarise(
#     intra_distance = mean(dist(st_coordinates(focus_geometry))),
#     .groups = "drop"
#   )
# 
# # Calcula distancias inter-cluster (Separação)
# centroids_matrix <- st_coordinates(cluster_data_summary$cluster_centroid)
# inter_cluster_distances <- as.matrix(dist(centroids_matrix))
# 
# mean_intra_distance <- mean(cluster_compactness$intra_distance)
# mean_inter_distance <- mean(
#   inter_cluster_distances[upper.tri(inter_cluster_distances)]
# )
# cat("Average intra-cluster distance: ", mean_intra_distance, "\n")
# cat("Average inter-cluster distance: ", mean_inter_distance, "\n")
# 
# # Um R > 1 indica clusters bem separados e compactos.
# mean_inter_distance/mean_intra_distance


library(tidyverse)
library(sf) # Objeto de coordenadas
library(dbscan) # Clusterização
library(lubridate)
library(leaflet) # Plot
library(leaflet.extras) # Plot
library(htmlwidgets) # Plot
library(stats) # Inter-Intra-clusters
library(data.table) # Load dataset
library(dplyr) # Load dataset

################################################################################
##### Dataset para o tempo até o término da queimada/ e tempo até recidiva #####
################################################################################
# Carregar e filtrar os dados
# df_inpe_part1 <- fread("focos_br_todos-sats_2020.csv", header = TRUE)
df_inpe_part2 <- fread("focos_br_todos-sats_2021.csv", header = TRUE)
df_inpe_part3 <- fread("focos_br_todos-sats_2022.csv", header = TRUE)
df_inpe_part4 <- fread("focos_br_todos-sats_2023.csv", header = TRUE)

# Combinar os datasets em um único dataframe
df_inpe_all <- rbind(
  # df_inpe_part1,
  df_inpe_part2,
  df_inpe_part3,
  df_inpe_part4
)

df_inpe2 <- subset(df_inpe_all, df_inpe_all$estado == "ESPÍRITO SANTO")

# install.packages("devtools")
# devtools::install_github("gdmcdonald/stdbscanr")
library(stdbscanr)

# Preparar dados para análise
fire_data <- df_inpe2 %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(32724) %>%  # CRS métrico (UTM 33S, 32633) (correto para ES = UTM 24S, EPSG:32724)
  mutate(
    X = st_coordinates(geometry)[, 1],  # Extrair X (coordenadas métricas)
    Y = st_coordinates(geometry)[, 2],  # Extrair Y (coordenadas métricas)
    time_in_hours = as.numeric(difftime(data_pas, min(data_pas), units = "hours"))
  )


# Determinar MinPts baseado na dimensionalidade dos dados
dimensionality <- 3  # X, Y, e time_in_hours
minpts_val <- 2 * dimensionality  # Regra geral para MinPts
cat("Valor inicial de MinPts definido como:", minpts_val, "\n")

# Selecionar as colunas X e Y, excluindo geometry
coords <- fire_data %>%
  st_drop_geometry() %>%  # Remove a coluna geometry (tipo list)
  dplyr::select(X, Y) %>%  # Seleciona apenas X e Y
  as.data.frame() %>%  # Converte para data.frame
  mutate(across(everything(), as.numeric)) %>%  # Garante que as colunas sejam numéricas
  as.matrix()  # Converte para matriz

# Verificar se a matriz é numérica
if (!is.numeric(coords)) {
  stop("Erro: As coordenadas precisam ser numéricas.")
}

# Calcular distâncias kNN com k = minpts_val - 1
kNN_distances <- dbscan::kNNdist(coords, k = minpts_val - 1)

# Ordenar as distâncias kNN
kNN_distances_sorted <- sort(kNN_distances)

# Definir os extremos da linha reta
x_start <- 1
x_end <- length(kNN_distances_sorted)
y_start <- kNN_distances_sorted[x_start]
y_end <- kNN_distances_sorted[x_end]

# Calcular a equação da linha reta (extremos)
line_slope <- (y_end - y_start) / (x_end - x_start)
line_intercept <- y_start - line_slope * x_start

# Calcular a distância perpendicular de cada ponto à linha reta
x_coords <- seq_along(kNN_distances_sorted)
y_coords <- kNN_distances_sorted
distances_to_line <- abs((line_slope * x_coords - y_coords + line_intercept) /
                           sqrt(line_slope^2 + 1))

# Identificar o índice do ponto de maior distância
inflection_index <- which.max(distances_to_line)
eps_inflexion <- kNN_distances_sorted[inflection_index]

# Plotar o gráfico com o ponto de inflexão destacado
plot(
  kNN_distances_sorted,
  type = "l",
  main = "kNN-dist Plot para Eps Espacial (Cotovelo Correto)",
  xlab = "Pontos Ordenados",
  ylab = "Distância"
)
abline(h = eps_inflexion, col = "red", lty = 2)  # Linha horizontal para o Eps escolhido
points(inflection_index, eps_inflexion, col = "blue", pch = 19)  # Destacar o ponto
lines(
  c(x_start, x_end),
  c(y_start, y_end),
  col = "green",
  lty = 2
)  # Adicionar a linha reta de referência

# Exibir o valor de Eps escolhido
cat("Valor de Eps identificado pelo ponto de inflexão:", eps_inflexion, "\n")


#------------------------------------------ MinPts para encontrar eps_t
# 1. Calcular o valor de MinPts sugerido
n <- nrow(fire_data)  # Número total de eventos
MinPts <- round(5 * log(n))
cat("Valor sugerido para MinPts:", MinPts, "\n")

# 2. Calcular a distância k-temporal (kNN temporal)
time_sorted <- sort(fire_data$time_in_hours)  # Ordenar os tempos
kNN_temporal <- dbscan::kNNdist(as.matrix(time_sorted), k = MinPts)

# Ordenar as distâncias kNN temporais em ordem decrescente
kNN_temporal_sorted <- sort(kNN_temporal, decreasing = TRUE)

# Definir os extremos da linha reta no gráfico
x_start <- 1
x_end <- length(kNN_temporal_sorted)
y_start <- kNN_temporal_sorted[x_start]
y_end <- kNN_temporal_sorted[x_end]

# Calcular a equação da linha reta (extremos)
line_slope <- (y_end - y_start) / (x_end - x_start)
line_intercept <- y_start - line_slope * x_start

# Calcular a distância perpendicular de cada ponto à linha reta
x_coords <- seq_along(kNN_temporal_sorted)
y_coords <- kNN_temporal_sorted
distances_to_line <- abs((line_slope * x_coords - y_coords + line_intercept) /
                           sqrt(line_slope^2 + 1))

# Identificar o índice do cotovelo (maior distância perpendicular)
cotovelo_index <- which.max(distances_to_line)
eps_t <- kNN_temporal_sorted[cotovelo_index]

# Plotar o gráfico com o ponto de cotovelo destacado
plot(
  kNN_temporal_sorted,
  type = "l",
  main = "kNN-dist Temporal Plot (Cotovelo)",
  xlab = "Eventos Ordenados (Decrescente)",
  ylab = "Distância Temporal (horas)"
)
abline(h = eps_t, col = "red", lty = 2)  # Linha horizontal para o eps_t
points(cotovelo_index, eps_t, col = "blue", pch = 19)  # Destacar o cotovelo
lines(
  c(x_start, x_end),
  c(y_start, y_end),
  col = "green",
  lty = 2
)  # Adicionar a linha reta de referência

# Exibir o valor de eps_t (25 horas)
cat("Limiar temporal eps_t identificado (cotovelo):", eps_t, "horas\n")

#----------------------- Modelo
eps = 1656
eps_t = 25
minpts = 6

# Aplicar ST-DBSCAN
clustered_data <- get_clusters_from_data(
  fire_data,
  x = "X",
  y = "Y",
  t = "time_in_hours",
  eps = eps,   # Distância espacial em metros
  eps_t = eps_t,  # Distância temporal em horas 
  minpts = minpts    # Mínimo de pontos para formar um cluster
)

# Filtrar clusters válidos (remover ruídos)
filtered_clusters <- clustered_data %>%
  filter(cluster > 0) %>%  # Remove pontos classificados como ruído
  group_by(cluster) %>%
  mutate(
    start_time = min(data_pas),
    end_time = max(data_pas),
    duration_hours = as.numeric(difftime(end_time, start_time, units = "hours"))
  ) %>%
  ungroup()


# Garantir que filtered_clusters seja um objeto sf e transformar para CRS geográfico
filtered_clusters <- filtered_clusters %>%
  st_as_sf() %>%  # Certifica que é sf
  st_transform(4326) %>%  # Transformar para CRS geográfico
  mutate(
    longitude = st_coordinates(geometry)[, 1],  # Longitude
    latitude = st_coordinates(geometry)[, 2]   # Latitude
  )

# Calcular Bounding Box e Centroids
cluster_summary <- filtered_clusters %>%
  group_by(cluster) %>%
  summarize(
    centroid = st_centroid(st_union(geometry)),  # Calcular centroide após unir geometria
    bbox = st_as_sfc(st_bbox(geometry))          # Calcular bounding box
  ) %>%
  ungroup() %>%
  st_as_sf() # Transformar para um objeto sf

# Converter Bounding Box para CRS correto
cluster_summary <- cluster_summary %>%
  mutate(
    bbox = st_transform(bbox, 4326),       # Transformar bbox para coordenadas geográficas
    centroid = st_transform(centroid, 4326) # Garantir que centroid esteja no mesmo CRS
  )


################## Intra-inter clusters
cluster_compactness <- filtered_clusters %>%
  group_by(cluster) %>%
  summarise(
    intra_distance = if (n() > 1) {
      mean(dist(st_coordinates(geometry)))
    } else {
      0  # Cluster com apenas um ponto
    },
    .groups = "drop"
  )

filtered_clusters <- filtered_clusters %>%
  filter(!is.na(geometry)) %>%  # Remover NAs
  mutate(focus_geometry = st_make_valid(geometry))  # Corrigir geometrias inválidas


# Calcular os centroides
centroids_matrix <- st_coordinates(cluster_summary$centroid)

# Calcular a separação média (inter-cluster)
inter_cluster_distances <- as.matrix(dist(centroids_matrix))
mean_inter_distance <- mean(inter_cluster_distances[upper.tri(inter_cluster_distances)])

# Calcular média global intra-cluster
mean_intra_distance <- mean(cluster_compactness$intra_distance, na.rm = TRUE)

# Razão global
global_ratio <- mean_inter_distance / mean_intra_distance

cat("Distância média intra-cluster: ", mean_intra_distance, "\n")
cat("Distância média inter-cluster: ", mean_inter_distance, "\n")
cat("Razão global (R): ", global_ratio, "\n")

########################
# Silhouette (normalizando APENAS para a métrica)
########################
# Precisamos de uma matriz de distâncias.
# Para evitar viés de escala, 'scale()' normaliza as coordenadas.

# Adicionar a dimensão temporal ao conjunto de variáveis
filtered_clusters_xyz <- clustered_data %>%
  filter(cluster > 0) %>%
  select(cluster, X, Y)

# Normalizar X, Y e tempo
scaled_xyz <- scale(filtered_clusters_xyz[, c("X", "Y")])

# Calcular a matriz de distâncias a partir de (X, Y, time_in_hours) normalizados
dist_matrix_scaled <- dist(scaled_xyz)

# Calcular o Silhouette com a matriz de distâncias ajustada
library(cluster)
labels <- filtered_clusters_xyz$cluster
sil <- silhouette(labels, dist_matrix_scaled)

# Plotar o gráfico de Silhouette
plot(sil, border = NA, main = "Silhouette Plot (com tempo incluído)")

# Calcular a largura média do Silhouette
avg_sil_width <- mean(sil[, 3])
cat("Silhouette médio (com tempo incluído): ", avg_sil_width, "\n")

########################
# Davies-Bouldin Index (usando coords normalizadas)
########################
library(clusterCrit)
db_index <- intCriteria(as.matrix(scaled_xyz), labels, c("Davies_Bouldin"))
cat("Davies-Bouldin Index: ", db_index[[1]], "\n")


# cluster_summary <- cluster_summary %>%
#   mutate(
#     bbox = st_as_sfc(bbox),  # Garantir que bbox seja um polígono sf
#     centroid = st_as_sfc(centroid)  # Garantir que centroid seja um ponto sf
#   )

#
 library(leaflet)

 # Criar mapa interativo com centroides, focos e delimitação aproximada dos clusters
 map <- leaflet() %>%
   addProviderTiles("CartoDB.Positron") %>%
   # Adicionar Delimitação Aproximada dos Clusters (Círculos ao redor dos centroides)
   addCircles(
     data = cluster_summary,
     lng = ~st_coordinates(centroid)[, 1],  # Longitude do centroide
     lat = ~st_coordinates(centroid)[, 2],  # Latitude do centroide
     radius = 1000,  # Raio em metros (ajuste conforme necessário)
     color = "blue",
     weight = 1,
     fillOpacity = 0.1,
     label = ~paste("Cluster ID: ", cluster),
     popup = ~paste(
       "Cluster ID: ", cluster,
       "<br>Centróide: ", paste(round(st_coordinates(centroid)[, 1], 6),
                                round(st_coordinates(centroid)[, 2], 6))
     )
   ) %>%
   # Adicionar Centroides
   addCircleMarkers(
     data = cluster_summary,
     lng = ~st_coordinates(centroid)[, 1],  # Longitude do centroide
     lat = ~st_coordinates(centroid)[, 2],  # Latitude do centroide
     color = "red",
     radius = 5,
     label = ~paste("Centróide do Cluster ID: ", cluster),
     popup = ~paste(
       "Cluster ID: ", cluster,
       "<br>Centróide: ", paste(round(st_coordinates(centroid)[, 1], 6),
                                round(st_coordinates(centroid)[, 2], 6))
     )
   ) %>%
   # Adicionar Focos de Incêndio
   addCircleMarkers(
     data = filtered_clusters,
     lng = ~longitude,  # Longitude
     lat = ~latitude,   # Latitude
     color = ~factor(cluster),  # Colorir por cluster
     radius = 3,
     opacity = 0.7,
     fillOpacity = 0.7,
     popup = ~paste(
       "Foco de Incêndio - Cluster ID: ", cluster,
       "<br>Coordenadas: ", paste(round(longitude, 6), round(latitude, 6)),
       "<br>Início: ", start_time,
       "<br>Fim: ", end_time,
       "<br>Duração: ", round(duration_hours, 2), " horas")
   )

 # Salvar o mapa
 saveWidget(map, "clusters_with_centroids_and_focus2.html")


 
 


 
 
 
 
 
  
#  # Extrair dados numéricos para cálculo
#  coords <- st_coordinates(filtered_clusters$geometry)
#  cluster_labels <- filtered_clusters$cluster
# 
#  # Calcular a silhueta
#  silhouette_scores <- silhouette(cluster_labels, dist(coords))
#  summary(silhouette_scores)
# 
#  # Plotar o gráfico de silhueta
#  plot(silhouette_scores)




# # Calcular as distâncias intra-cluster (compactação) para cada cluster
# cluster_compactness <- filtered_clusters %>%
#   group_by(cluster_estado) %>%
#   summarise(
#     intra_distance = mean(dist(st_coordinates(focus_geometry))),
#     .groups = "drop"
#   )
# 
# # Calcular as distâncias entre os centroides dos clusters (inter-cluster)
# centroids_matrix <- st_coordinates(cluster_data_summary$cluster_centroid)
# inter_cluster_distances <- as.matrix(dist(centroids_matrix))
# 
# # Obter os estados (clusters) para referência
# cluster_states <- cluster_compactness$cluster_estado
# 
# # Inicializar o vetor para armazenar os valores máximos de R_i
# R_i <- numeric(length(cluster_states))
# 
# # Calcular o DB Index
# for (i in seq_along(cluster_states)) {
#   # Dispersão do cluster i
#   s_i <- cluster_compactness$intra_distance[i]
#   
#   # Iterar sobre os demais clusters para calcular R_ij
#   R_ij <- sapply(seq_along(cluster_states), function(j) {
#     if (i != j) {
#       s_j <- cluster_compactness$intra_distance[j]
#       d_ij <- inter_cluster_distances[i, j]
#       return((s_i + s_j) / d_ij)
#     } else {
#       return(NA) # Ignorar o próprio cluster
#     }
#   })
#   
#   # Armazenar o máximo R_ij para o cluster i
#   R_i[i] <- max(R_ij, na.rm = TRUE)
# }
# 
# # Calcular o índice de Davies-Bouldin como a média de R_i
# DB_index <- mean(R_i)
# 
# cat("Davies-Bouldin Index: ", DB_index, "\n")



# Criar mapa interativo
map <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  # Adicionar Bounding Boxes
  addPolygons(
    data = cluster_data_summary,
    color = "blue",
    weight = 2,
    opacity = 0.7,
    fillOpacity = 0.1,
    label = ~paste("Cluster ID: ", cluster_estado),
    popup = ~paste("Cluster ID: ", cluster_estado)
  ) %>%
  # Adicionar Centroids
  addCircleMarkers(
    data = centroids,
    color = "red",
    radius = 5,
    label = ~paste("Centróide do Cluster ID: ", cluster_estado),
    popup = ~paste(
      "Cluster ID: ", cluster_estado,
      "<br>Centróide: ", geometry
    )
  ) %>%
  # Adicionar Focos de Incêndio Individuais
  addCircleMarkers(
    data = filtered_clusters,
    color = ~factor(cluster_estado),
    radius = 3,
    opacity = 0.7,
    fillOpacity = 0.7,
    popup = ~paste(
      "Foco de Incêndio - Cluster ID: ", cluster_estado,
      "<br>Município: ", municipio,
      "<br>Coordenadas: ", focus_geometry,
      "<br>Início: ", start_time, 
      "<br>Fim: ", end_time, 
      "<br>Duração: ", round(time_diff / 60, 2), " horas")
  )

# Salvar o mapa
saveWidget(map, "clusters_with_bbox_and_centroids.html")

# Mapa com clusterização automática
map <- leaflet(filtered_clusters) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addMarkers(
    # Ativar clusterização automática
    clusterOptions = markerClusterOptions(),  
    popup = ~paste(
      "Cluster (Estado): ", cluster_estado, 
      "<br>Município: ", municipio,
      "<br>Coordenadas: ", focus_geometry,
      "<br>Início: ", start_time, 
      "<br>Fim: ", end_time, 
      "<br>Duração: ", round(time_diff / 60, 2), " horas"
    )
  )

# Salvar o mapa
saveWidget(map, "clusters_marker_clustered.html")

# Categorizar clusters por tamanho
filtered_clusters <- filtered_clusters %>%
  group_by(cluster_estado) %>%
  mutate(
    # Número de focos no cluster
    cluster_size = n(),  
    cluster_size_category = case_when(
      cluster_size < 10 ~ "Pequeno (05-09 focos)",
      cluster_size >= 10 & cluster_size < 25 ~ "Médio (10-24 focos)",
      cluster_size >= 25 ~ "Grande (≥ 25 focos)"
    )
  ) %>%
  ungroup()

# Criar uma paleta para as categorias
palette <- colorFactor(
  # Cores para cada categoria
  palette = c("purple", "blue", "lightblue"),  
  domain = c("Pequeno (05-09 focos)", "Médio (10-24 focos)", "Grande (≥ 25 focos)")
)

# Criar o mapa por categorias de tamanho
map <- leaflet(filtered_clusters) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(
    # Usar cores baseadas no tamanho do cluster
    color = ~palette(cluster_size_category),  
    radius = 5,
    opacity = 0.7,
    fillOpacity = 0.7,
    popup = ~paste(
      "Cluster (Estado): ", cluster_estado,
      "<br>Município: ", municipio,
      "<br>Tamanho: ", cluster_size,
      "<br>Coordenadas: ", focus_geometry,
      "<br>Categoria: ", cluster_size_category,
      "<br>Início: ", start_time,
      "<br>Fim: ", end_time,
      "<br>Duração: ", round(time_diff / 60, 2), " horas"
    )
  ) %>%
  addLegend(
    position = "topright",
    title = "Tamanho do Cluster",
    pal = palette,
    values = ~cluster_size_category
  )

# Salvar o mapa
saveWidget(map, "clusters_by_size.html")
#-------------------------------------------------------------------------------
# Dataset
unique_clusters <- filtered_clusters %>%
  group_by(cluster_estado) %>%
  summarise(
    # Use o primeiro centróide
    centroid_geometry = first(centroid_geometry)  
  ) %>%
  ungroup() %>%
  # Manter como objeto sf para operações espaciais
  st_as_sf()  

# Garantir que o CRS seja métrico
unique_clusters <- st_transform(unique_clusters, crs = 32633)

# Extrair coordenadas espaciais para agrupamento
data <- st_coordinates(unique_clusters$centroid_geometry)

# # Faz o Elbow plot
# coords <- data
# kdist <- kNNdist(coords, k = 5)
# plot(
#   sort(kdist), type = "l", main = "Gráfico de k-Distância",
#   xlab = "Pontos Ordenados", ylab = "Distância"
#   )
# abline(h = 1000, col = "red", lty = 2)  # Linha indicando um eps sugerido

# Definir parâmetros de agrupamento espacial
eps_spatial <- 12000  # Spatial threshold em metros
minPts <- 4           # Pontos mínimos para formar um cluster

# Aplicar DBSCAN para agrupamento espacial
spatial_clusters <- dbscan(data, eps = eps_spatial, minPts = minPts)

# dataset com superclusters + cluster_estado, etc
unique_clusters <- unique_clusters %>%
  mutate(supercluster_id = spatial_clusters$cluster) %>%
  filter(supercluster_id > 0)  # Remove ruído (cluster ID = 0)

# superclusters
superclusters <- unique_clusters %>%
  group_by(supercluster_id) %>%
  summarise(
    # Usa convex box para a caixa delimitadora
    supercluster_bbox = st_convex_hull(st_union(centroid_geometry)), 
    # Calcular novo centróide
    supercluster_centroid = st_centroid(st_union(centroid_geometry)),  
    centroid_count = n()  # Número de centróides no superclusters
  ) %>%
  st_as_sf()

# Garantir a consistência do CRS para visualização
superclusters <- superclusters %>%
  mutate(
    supercluster_centroid = st_transform(supercluster_centroid, crs = 4326),
    supercluster_bbox = st_transform(supercluster_bbox, crs = 4326)
  )
unique_clusters <- st_transform(unique_clusters, crs = 4326)

# Compacidade (distâncias intra-cluster)
supercluster_compactness <- unique_clusters %>%
  group_by(supercluster_id) %>%
  summarise(
    intra_distance = mean(dist(st_coordinates(centroid_geometry))),
    .groups = "drop"
  )

# Separação (distâncias entre grupos)
supercluster_data <- st_coordinates(superclusters$supercluster_centroid)
inter_supercluster_distances <- as.matrix(dist(supercluster_data))

mean_intra_distance <- mean(
  supercluster_compactness$intra_distance, na.rm = TRUE
  )
mean_inter_distance <- mean(
  inter_supercluster_distances[upper.tri(inter_supercluster_distances)]
  )

# Métricas de Validação
cat("Average intra-supercluster distance: ", mean_intra_distance, "\n")
cat("Average inter-supercluster distance: ", mean_inter_distance, "\n")

# Mapa interativo para superclusters
map <- leaflet(superclusters) %>%
  addProviderTiles("CartoDB.Positron") %>%
  # Adiciona bounding boxes para os superclusters
  addPolygons(
    data = superclusters,
    color = "blue",
    weight = 2,
    opacity = 0.7,
    fillOpacity = 0.1,
    label = ~paste("Supercluster ID: ", supercluster_id),
    popup = ~paste(
      "Supercluster ID: ", supercluster_id,
      "<br>Centroid Count: ", centroid_count
    )
  ) %>%
  # Adiciona centroids dos superclusters 
  addCircleMarkers(
    lng = ~st_coordinates(supercluster_centroid)[, 1],  # Longitude
    lat = ~st_coordinates(supercluster_centroid)[, 2],  # Latitude
    color = "red",
    radius = 6,
    label = ~paste("Supercluster ID: ", supercluster_id),
    popup = ~paste(
      "Supercluster ID: ", supercluster_id,
      "<br>Centroid Count: ", centroid_count,
      "<br>Centroid: (", round(st_coordinates(supercluster_centroid)[, 1], 5), 
      ", ", round(st_coordinates(supercluster_centroid)[, 2], 5), ")"
    )
  ) %>%
  # Adiciona centróides dos clusters(de focos)/incendios
  addCircleMarkers(
    lng = ~st_coordinates(centroid_geometry)[, 1],  # Longitude
    lat = ~st_coordinates(centroid_geometry)[, 2],  # Latitude
    data = unique_clusters,
    color = "green",
    radius = 4,
    label = ~paste("Cluster ID: ", cluster_estado),
    popup = ~paste(
      "Cluster ID: ", cluster_estado,
      "<br>Centroid: (", round(st_coordinates(centroid_geometry)[, 1], 5), 
      ", ", round(st_coordinates(centroid_geometry)[, 2], 5), ")"
    )
  )

# Salve o mapa
saveWidget(map, "Superclusters_with_FireFoci_Clusters.html")


# # Plot com clusters de tamanho 1
# superclusters <- superclusters %>%
#   mutate(
#     supercluster_bbox = if_else(
#       centroid_count == 1,
#       st_buffer(supercluster_centroid, dist = 100),  # 100 meters buffer
#       supercluster_bbox
#     )
#   )
# 
# map <- leaflet(superclusters) %>%
#   addProviderTiles("CartoDB.Positron") %>%
#   # Add bounding boxes for superclusters
#   addPolygons(
#     data = superclusters,
#     color = "blue",
#     weight = 2,
#     opacity = 0.7,
#     fillOpacity = 0.1,
#     label = ~paste("Supercluster ID: ", supercluster_id),
#     popup = ~paste(
#       "Supercluster ID: ", supercluster_id,
#       "<br>Centroid Count: ", centroid_count
#     )
#   ) %>%
#   # Add supercluster centroids
#   addCircleMarkers(
#     lng = ~st_coordinates(supercluster_centroid)[, 1],  
#     lat = ~st_coordinates(supercluster_centroid)[, 2],  
#     color = "red",
#     radius = 6,
#     label = ~paste("Supercluster ID: ", supercluster_id),
#     popup = ~paste(
#       "Supercluster ID: ", supercluster_id,
#       "<br>Centroid Count: ", centroid_count,
#       "<br>Centroid: (", round(st_coordinates(supercluster_centroid)[, 1], 5), 
#       ", ", round(st_coordinates(supercluster_centroid)[, 2], 5), ")"
#     )
#   ) %>%
#   # Add centroids of fire foci clusters
#   addCircleMarkers(
#     lng = ~st_coordinates(centroid_geometry)[, 1],  
#     lat = ~st_coordinates(centroid_geometry)[, 2],  
#     data = unique_clusters,
#     color = "green",
#     radius = 4,
#     label = ~paste("Cluster ID: ", cluster_estado),
#     popup = ~paste(
#       "Cluster ID: ", cluster_estado,
#       "<br>Centroid: (", round(st_coordinates(centroid_geometry)[, 1], 5), 
#       ", ", round(st_coordinates(centroid_geometry)[, 2], 5), ")"
#     )
#   )
# 
# # Save the map
# saveWidget(map, "Superclusters_with_FireFoci_Clusters_Buffer.html")

# Adicionar variáveis de superclusters para unique_clusters 
#usando supercluster_id
superclusters_non_geom <- superclusters %>%
  as.data.frame() %>%  # Converter para dataframe, eliminando a geometria
  select(
    supercluster_id, supercluster_bbox, 
    supercluster_centroid, centroid_count
    )

regioes_dataset <- unique_clusters %>%
  left_join(superclusters_non_geom, by = "supercluster_id")


# Faz join usando cluster_estado
final_dataset <- filtered_clusters %>%
  left_join(
    regioes_dataset %>%
      st_drop_geometry() %>%  # Drop geometry para simplificar a junção
      select(
        cluster_estado, supercluster_id, supercluster_bbox, 
        supercluster_centroid, centroid_count
        ),
    by = "cluster_estado"
  )
#-------------------------------------------------------------------------------
# Colapsa o conjunto de dados em clusters exclusivos dentro de cada supercluster
final_dataset2 <- final_dataset %>%
  select(supercluster_id, cluster_estado, start_time, end_time) %>%
  distinct() %>%  # Remover duplicatas
  arrange(supercluster_id, start_time)  # Garantir a ordenação por tempo

# Extraia a última linha para cada grupo
final_dataset2 <- final_dataset2 %>%
  group_by(supercluster_id, cluster_estado) %>%
  slice_tail(n = 1) %>%  # Pegue a última linha de cada grupo
  ungroup()  # Remova o agrupamento se forem necessárias mais operações

# Calcule o tempo até o próximo incêndio dentro de cada supercluster
relapse_data <- final_dataset2 %>%
  group_by(supercluster_id) %>%
  mutate(
    # Tempo até o próximo incêndio no mesmo supercluster
    time_until_next_fire = as.numeric(
      difftime(lead(start_time), end_time, units = "hours")
    ),
    # Lidar com intervalos negativos ou sobrepostos
    time_until_next_fire = ifelse(
      time_until_next_fire <= 0, NA, time_until_next_fire
      ),
    # Defina o indicador de relapse
    relapse_indicator = ifelse(is.na(time_until_next_fire), 0, 1)
  ) %>%
  ungroup()

relapse_data <- relapse_data %>%
  filter(!is.na(supercluster_id))

# Resumir estatísticas de recaída para cada supercluster
supercluster_relapse_summary <- relapse_data %>%
  group_by(supercluster_id) %>%
  summarise(
    avg_time_until_next_fire = mean(time_until_next_fire, na.rm = TRUE),  
    median_time_until_next_fire = median(time_until_next_fire, na.rm = TRUE),  
    max_time_until_next_fire = max(time_until_next_fire, na.rm = TRUE),  
    min_time_until_next_fire = min(time_until_next_fire, na.rm = TRUE),  
    fire_count = n()  # Total number of unique fires in the region
  ) %>%
  ungroup()

# Solte temporariamente a geometria de superclusters
superclusters_non_geom <- superclusters %>%
  st_drop_geometry()  # Drop geometria para non-spatial join

# Join
final_superclusters <- superclusters_non_geom %>%
  left_join(supercluster_relapse_summary, by = "supercluster_id") %>%
  st_as_sf(geometry = st_geometry(superclusters))  # Restaurar geometria


# Visualizar superclusters com os tempos de relapse 
map <- leaflet(final_superclusters) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    color = "blue",
    weight = 2,
    opacity = 0.7,
    fillOpacity = 0.1,
    label = ~paste("Supercluster ID: ", supercluster_id),
    popup = ~paste(
      "Supercluster ID: ", supercluster_id,
      "<br>Min Time Until Next Fire: ", round(
        min_time_until_next_fire, 2
        ), " hours",
      "<br>Average Time Until Next Fire: ", round(
        avg_time_until_next_fire, 2
        ), " hours",
      "<br>Median Time Until Next Fire: ", round(
        median_time_until_next_fire, 2
        ), " hours",
      "<br>Max Time Until Next Fire: ", round(
        max_time_until_next_fire, 2
        ), " hours",
      "<br>Fire Count: ", fire_count
    )
  )

# Salvar o mapa
saveWidget(map, "superclusters_with_relapse_times.html")


# Salva dados 
saveRDS(relapse_data, file = "relapse_data.rds")
loaded_relapse_data <- readRDS("relapse_data.rds")



