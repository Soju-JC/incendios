library(tidyverse)
library(sf) # objeto de coordenadas 
library(dbscan) # clusterização
library(lubridate)
library(leaflet) # plot
library(leaflet.extras) # plot
library(htmlwidgets) # plot

# Carregar e filtrar os dados
df_inpe <- read.csv("focos_br_todos-sats_2023.csv", header = TRUE)
df_inpe2 <- subset(df_inpe, df_inpe$estado == "ESPÍRITO SANTO")

# Converter para sf e transformar CRS para métrico
fire_data <- st_as_sf(df_inpe2, coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = 32633)

# # Extrair coordenadas
# coords <- st_coordinates(fire_data)
# 
# # k-distância para 5 vizinhos
# kdist <- kNNdist(coords, k = 5)
# 
# # Plotar k-distância
# plot(sort(kdist), type = "l", main = "Gráfico de k-Distância", xlab = "Pontos Ordenados", ylab = "Distância")
# abline(h = 1000, col = "red", lty = 2)  # Linha indicando um eps sugerido

# Configurações de DBSCAN
eps_spatial <- 1000   # Distância máxima espacial em metros
eps_temporal <- 5 * 24   # 5 dias em horas 

# Clustering espacial e temporal
clustered_data <- fire_data %>%
  group_by(municipio) %>%
  group_modify(~ {
    coords <- st_coordinates(.x)
    time_in_hours <- as.numeric(difftime(.x$data_pas, min(.x$data_pas), units = "hours"))
    time_scaled <- time_in_hours * (eps_spatial / eps_temporal)
    combined_data <- cbind(coords, time_scaled)
    clusters <- dbscan(combined_data, eps = eps_spatial, minPts = 3)
    .x$cluster_municipio <- clusters$cluster
    return(.x)
  }) %>%
  ungroup() %>%
  st_as_sf(crs = 32633) %>%  # Reatribuir classe sf após manipulação
  filter(cluster_municipio > 0) %>%
  group_by(municipio, cluster_municipio) %>%
  mutate(cluster_estado = cur_group_id()) %>%
  ungroup()

# Adicionar informações temporais por cluster
filtered_clusters <- clustered_data %>%
  group_by(cluster_municipio) %>%
  mutate(
    start_time = min(data_pas),
    end_time = max(data_pas),
    time_diff = as.numeric(difftime(end_time, start_time, units = "mins"))
  ) %>%
  filter(time_diff > 0) %>%  # Remover clusters inválidos
  ungroup() %>%
  mutate(duration = round(time_diff / 60, 2))  # Duração em horas

# Calcular Bounding Box e centroids
cluster_data_summary <- filtered_clusters %>%
  group_by(cluster_municipio) %>%
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

# # Verificar CRS (opcional, para garantir que está em EPSG:4326)
# st_crs(centroids)

# Renomear a coluna geometry para focus_geometry
filtered_clusters <- filtered_clusters %>%
  rename(focus_geometry = geometry)

# Adicionar geometrias de centroid e bounding box como variáveis separadas
filtered_clusters <- filtered_clusters %>%
  group_by(cluster_municipio) %>%
  mutate(
    # Calcular geometria do centroide
    centroid_geometry = st_centroid(st_union(focus_geometry)),
    # Calcular geometria da bounding box
    bbox_geometry = st_as_sfc(st_bbox(focus_geometry))          
  ) %>%
  ungroup()

# Garantir transformação de filtered_clusters e centroids para EPSG:4326
filtered_clusters <- st_transform(filtered_clusters, crs = 4326)
centroids <- st_transform(centroids, crs = 4326)

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
    label = ~paste("Cluster ID: ", cluster_municipio),
    popup = ~paste("Cluster ID: ", cluster_municipio)
  ) %>%
  # Adicionar Centroids
  addCircleMarkers(
    data = centroids,
    color = "red",
    radius = 5,
    label = ~paste("Centróide do Cluster ID: ", cluster_municipio),
    popup = ~paste(
      "Cluster ID: ", cluster_municipio,
      "<br>Centróide: (", round(st_coordinates(geometry)[1], 5), 
      ", ", round(st_coordinates(geometry)[2], 5), ")"
    )
  ) %>%
  # Adicionar Focos de Incêndio Individuais
  addCircleMarkers(
    data = filtered_clusters,
    color = ~factor(cluster_municipio),
    radius = 3,
    opacity = 0.7,
    fillOpacity = 0.7,
    popup = ~paste("Foco de Incêndio - Cluster ID: ", cluster_municipio)
  )

# Salvar o mapa
saveWidget(map, "Muni_clusters_with_bbox_and_centroids.html")

# Mapa com clusterização automática
map <- leaflet(filtered_clusters) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addMarkers(
    clusterOptions = markerClusterOptions(),  # Ativar clusterização automática
    popup = ~paste(
      "Cluster (Município): ", cluster_municipio, 
      "<br>Município: ", municipio, 
      "<br>Início: ", start_time, 
      "<br>Fim: ", end_time, 
      "<br>Duração: ", round(time_diff / 60, 2), " horas"
    )
  )

# Salvar o mapa
saveWidget(map, "Muni_clusters_marker_clustered.html")

# Categorizar clusters por tamanho
filtered_clusters <- filtered_clusters %>%
  group_by(cluster_municipio) %>%
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
      "Cluster (Município): ", cluster_municipio,
      "<br>Tamanho: ", cluster_size,
      "<br>Categoria: ", cluster_size_category,
      "<br>Município: ", municipio,
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
saveWidget(map, "Muni_clusters_by_size.html")
