library(tidyverse)
library(sf) # Objeto de coordenadas 
library(dbscan) # Clusterização
library(lubridate)
library(leaflet) # Plot
library(leaflet.extras) # Plot
library(htmlwidgets) # Plot

# Carregar e filtrar os dados
# df_inpe_part1 <- read.csv("focos_br_todos-sats_2021.csv", header = TRUE)
df_inpe_part2 <- read.csv("focos_br_todos-sats_2022.csv", header = TRUE)
df_inpe_part3 <- read.csv("focos_br_todos-sats_2023.csv", header = TRUE)

# Combinar os datasets em um único dataframe
df_inpe_all <- rbind(df_inpe_part2, df_inpe_part3)

df_inpe2 <- subset(df_inpe_all, df_inpe_all$estado == "ESPÍRITO SANTO")

# Converter para sf e transformar CRS para métrico
fire_data <- st_as_sf(df_inpe2, coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = 32633)

# Configurações de DBSCAN
eps_spatial <- 1000   # Distância máxima espacial em metros
eps_temporal <- 5 * 24   # 5 dias em horas 

# Clustering espacial e temporal sem limitar por município
clustered_data <- fire_data %>%
  mutate(
    time_in_hours = as.numeric(difftime(data_pas, min(data_pas), units = "hours")),
    time_scaled = time_in_hours * (eps_spatial / eps_temporal)
  ) %>%
  group_modify(~ {
    coords <- st_coordinates(.x)
    combined_data <- cbind(coords, .x$time_scaled)
    clusters <- dbscan(combined_data, eps = eps_spatial, minPts = 5)
    .x$cluster_estado <- clusters$cluster
    return(.x)
  }) %>%
  ungroup() %>%
  filter(cluster_estado > 0) %>%  # Remover clusters de ruído (0)
  st_as_sf(crs = 32633)          # Reatribuir CRS métrico

# Adicionar informações temporais por cluster
filtered_clusters <- clustered_data %>%
  group_by(cluster_estado) %>%
  mutate(
    start_time = min(data_pas),
    end_time = max(data_pas),
    time_diff = as.numeric(difftime(end_time, start_time, units = "mins"))
  ) %>%
  filter(time_diff > 0) %>%  # Remover clusters inválidos
  ungroup() %>%
  mutate(duration = round(time_diff / 60, 2))  # Duração em horas

# Calcular Bounding Box e centroids para clusters estaduais
cluster_data_summary <- filtered_clusters %>%
  group_by(cluster_estado) %>%
  summarize(
    cluster_bbox = st_as_sfc(st_bbox(geometry)),
    cluster_centroid = st_centroid(st_union(geometry))
  ) %>%
  ungroup() %>%
  st_as_sf() %>%
  st_transform(crs = 4326)  # Transformar para EPSG:4326

# centroides como um sf separado
centroids <- cluster_data_summary %>%
  st_drop_geometry() %>%
  mutate(geometry = st_geometry(cluster_centroid)) %>%  # Usar geometria existente
  st_as_sf()  # Não redefinir CRS desnecessariamente

# Renomear a coluna geometry para focus_geometry
filtered_clusters <- filtered_clusters %>%
  rename(focus_geometry = geometry)

# Adicionar geometrias de centroid e bounding box como variáveis separadas
filtered_clusters <- filtered_clusters %>%
  group_by(cluster_estado) %>%
  mutate(
    # Calcular geometria do centroide
    centroid_geometry = st_centroid(st_union(focus_geometry)),
    # Calcular geometria da bounding box
    bbox_geometry = st_as_sfc(st_bbox(focus_geometry))          
  ) %>%
  ungroup()

# Verifica o CRS
st_crs(filtered_clusters)

# # Garantir transformação de filtered_clusters e centroids para EPSG:4326
filtered_clusters <- filtered_clusters %>%
  mutate(
    focus_geometry = st_transform(focus_geometry, crs = 4326),
    centroid_geometry = st_transform(centroid_geometry, crs = 4326),
    bbox_geometry = st_transform(bbox_geometry, crs = 4326)
  )

centroids <- centroids %>%
  mutate(
    cluster_centroid = st_transform(cluster_centroid, crs = 4326),
    geometry = st_transform(geometry, crs = 4326)
  )

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
saveWidget(map, "State_clusters_with_bbox_and_centroids.html")

# Mapa com clusterização automática
map <- leaflet(filtered_clusters) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addMarkers(
    clusterOptions = markerClusterOptions(),  # Ativar clusterização automática
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
saveWidget(map, "State_clusters_marker_clustered.html")

# Categorizar clusters por tamanho
filtered_clusters <- filtered_clusters %>%
  group_by(cluster_estado) %>%
  mutate(
    cluster_size = n(),  # Número de focos no cluster
    cluster_size_category = case_when(
      cluster_size < 10 ~ "Pequeno (01-09 focos)",
      cluster_size >= 10 & cluster_size < 25 ~ "Médio (10-24 focos)",
      cluster_size >= 25 ~ "Grande (≥ 25 focos)"
    )
  ) %>%
  ungroup()

# Criar uma paleta para as categorias
palette <- colorFactor(
  palette = c("purple", "blue", "lightblue"),  # Cores para cada categoria
  domain = c("Pequeno (01-09 focos)", "Médio (10-24 focos)", "Grande (≥ 25 focos)")
)

# Criar o mapa por categorias de tamanho
map <- leaflet(filtered_clusters) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(
    color = ~palette(cluster_size_category),  # Usar cores baseadas no tamanho do cluster
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
saveWidget(map, "State_clusters_by_size.html")


################ Validação ################
# Calculate intra-cluster distances (compactness)
library(stats)
cluster_compactness <- filtered_clusters %>%
  group_by(cluster_estado) %>%
  summarise(
    intra_distance = mean(dist(st_coordinates(focus_geometry))),
    .groups = "drop"
  )

# Calculate inter-cluster distances (separation)
centroids_matrix <- st_coordinates(cluster_data_summary$cluster_centroid)
inter_cluster_distances <- as.matrix(dist(centroids_matrix))

# Summary statistics
mean_intra_distance <- mean(cluster_compactness$intra_distance)
mean_inter_distance <- mean(inter_cluster_distances[upper.tri(inter_cluster_distances)])
cat("Average intra-cluster distance: ", mean_intra_distance, "\n")
cat("Average inter-cluster distance: ", mean_inter_distance, "\n")

# Calculate temporal ranges within each cluster
temporal_validation <- filtered_clusters %>%
  group_by(cluster_estado) %>%
  summarise(
    start_time = min(data_pas),
    end_time = max(data_pas),
    time_span = as.numeric(difftime(max(data_pas), min(data_pas), units = "days"))
  )

# Display clusters with time spans
print(temporal_validation)

# Visualize time spans
ggplot(temporal_validation, aes(x = cluster_estado, y = time_span)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Cluster Time Spans", x = "Cluster ID", y = "Time Span (days)")


# library(tidyverse)
# library(sf) # objeto de coordenadas 
# library(dbscan) # clusterização
# library(lubridate)
# library(leaflet) # plot
# library(leaflet.extras) # plot
# library(htmlwidgets) # plot
# 
# # Carregar e filtrar os dados
# df_inpe <- read.csv("focos_br_todos-sats_2023.csv", header = TRUE)
# df_inpe2 <- subset(df_inpe, df_inpe$estado == "ESPÍRITO SANTO")
# 
# # Converter para sf e transformar CRS para métrico
# fire_data <- st_as_sf(df_inpe2, coords = c("longitude", "latitude"), crs = 4326) %>%
#   st_transform(crs = 32633)
# 
# # Configurações de DBSCAN
# eps_spatial <- 1000   # Distância máxima espacial em metros
# eps_temporal <- 5 * 24   # 5 dias em horas 
# 
# # Clustering espacial e temporal
# clustered_data <- fire_data %>%
#   group_by(municipio) %>%
#   group_modify(~ {
#     coords <- st_coordinates(.x)
#     time_in_hours <- as.numeric(difftime(.x$data_pas, min(.x$data_pas), units = "hours"))
#     time_scaled <- time_in_hours * (eps_spatial / eps_temporal)
#     combined_data <- cbind(coords, time_scaled)
#     clusters <- dbscan(combined_data, eps = eps_spatial, minPts = 3)
#     .x$cluster_municipio <- clusters$cluster
#     return(.x)
#   }) %>%
#   ungroup() %>%
#   st_as_sf(crs = 32633) %>%  # Reatribuir classe sf após manipulação
#   filter(cluster_municipio > 0) %>%
#   group_by(municipio, cluster_municipio) %>%
#   mutate(cluster_estado = cur_group_id()) %>%
#   ungroup()
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
# # Calcular Bounding Box e centroids
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
#   mutate(geometry = st_geometry(cluster_centroid)) %>%  # Usar geometria existente
#   st_as_sf()  # Não redefinir CRS desnecessariamente
# 
# # # Verificar CRS (opcional, para garantir que está em EPSG:4326)
# # st_crs(centroids)
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
# # Garantir transformação de filtered_clusters e centroids para EPSG:4326
# filtered_clusters <- st_transform(filtered_clusters, crs = 4326)
# centroids <- st_transform(centroids, crs = 4326)
# 
# # Criar mapa interativo
# map <- leaflet() %>%
#   addProviderTiles("CartoDB.Positron") %>%
#   # Adicionar Bounding Boxes
#   addPolygons(
#     data = cluster_data_summary,
#     color = "blue",
#     weight = 2,
#     opacity = 0.7,
#     fillOpacity = 0.1,
#     label = ~paste("Cluster ID: ", cluster_estado),
#     popup = ~paste("Cluster ID: ", cluster_estado)
#   ) %>%
#   # Adicionar Centroids
#   addCircleMarkers(
#     data = centroids,
#     color = "red",
#     radius = 5,
#     label = ~paste("Centróide do Cluster ID: ", cluster_estado),
#     popup = ~paste(
#       "Cluster ID: ", cluster_estado,
#       "<br>Centróide: (", round(st_coordinates(geometry)[1], 5), 
#       ", ", round(st_coordinates(geometry)[2], 5), ")"
#     )
#   ) %>%
#   # Adicionar Focos de Incêndio Individuais
#   addCircleMarkers(
#     data = filtered_clusters,
#     color = ~factor(cluster_estado),
#     radius = 3,
#     opacity = 0.7,
#     fillOpacity = 0.7,
#     popup = ~paste("Foco de Incêndio - Cluster ID: ", cluster_estado)
#   )
# 
# # Salvar o mapa
# saveWidget(map, "State_clusters_with_bbox_and_centroids.html")
# 
# # Mapa com clusterização automática
# map <- leaflet(filtered_clusters) %>%
#   addProviderTiles("CartoDB.Positron") %>%
#   addMarkers(
#     clusterOptions = markerClusterOptions(),  # Ativar clusterização automática
#     popup = ~paste(
#       "Cluster (Estado): ", cluster_estado, 
#       "<br>Município: ", municipio, 
#       "<br>Início: ", start_time, 
#       "<br>Fim: ", end_time, 
#       "<br>Duração: ", round(time_diff / 60, 2), " horas"
#     )
#   )
# 
# # Salvar o mapa
# saveWidget(map, "State_clusters_marker_clustered.html")
# 
# # Categorizar clusters por tamanho
# filtered_clusters <- filtered_clusters %>%
#   group_by(cluster_estado) %>%
#   mutate(
#     cluster_size = n(),  # Número de focos no cluster
#     cluster_size_category = case_when(
#       cluster_size < 10 ~ "Pequeno (01-09 focos)",
#       cluster_size >= 10 & cluster_size < 25 ~ "Médio (10-24 focos)",
#       cluster_size >= 25 ~ "Grande (≥ 25 focos)"
#     )
#   ) %>%
#   ungroup()
# 
# # Criar uma paleta para as categorias
# palette <- colorFactor(
#   palette = c("purple", "blue", "lightblue"),  # Cores para cada categoria
#   domain = c("Pequeno (01-09 focos)", "Médio (10-24 focos)", "Grande (≥ 25 focos)")
# )
# 
# # Criar o mapa por categorias de tamanho
# map <- leaflet(filtered_clusters) %>%
#   addProviderTiles("CartoDB.Positron") %>%
#   addCircleMarkers(
#     color = ~palette(cluster_size_category),  # Usar cores baseadas no tamanho do cluster
#     radius = 5,
#     opacity = 0.7,
#     fillOpacity = 0.7,
#     popup = ~paste(
#       "Cluster (Estado): ", cluster_estado,
#       "<br>Tamanho: ", cluster_size,
#       "<br>Categoria: ", cluster_size_category,
#       "<br>Município: ", municipio,
#       "<br>Início: ", start_time,
#       "<br>Fim: ", end_time,
#       "<br>Duração: ", round(time_diff / 60, 2), " horas"
#     )
#   ) %>%
#   addLegend(
#     position = "topright",
#     title = "Tamanho do Cluster",
#     pal = palette,
#     values = ~cluster_size_category
#   )
# 
# # Salvar o mapa
# saveWidget(map, "State_clusters_by_size.html")
