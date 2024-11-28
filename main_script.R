library(tidyverse)
library(sf) # Objeto de coordenadas 
library(dbscan) # Clusterização
library(lubridate)
library(leaflet) # Plot
library(leaflet.extras) # Plot
library(htmlwidgets) # Plot
library(stats) # Inter-Intra-clusters

################ Dataset para o tempo até o término da queimada ################
#-------------------------------------------------------------------------------
# Carregar e filtrar os dados
# df_inpe_part1 <- read.csv("focos_br_todos-sats_2021.csv", header = TRUE)
df_inpe_part2 <- read.csv("focos_br_todos-sats_2022.csv", header = TRUE)
df_inpe_part3 <- read.csv("focos_br_todos-sats_2023.csv", header = TRUE)

# Combinar os datasets em um único dataframe
df_inpe_all <- rbind(df_inpe_part2, df_inpe_part3)

df_inpe2 <- subset(df_inpe_all, df_inpe_all$estado == "ESPÍRITO SANTO")

# Converter para sf e transformar CRS para métrico
fire_data <- st_as_sf(
  df_inpe2, coords = c("longitude", "latitude"), crs = 4326
  ) %>%
  st_transform(crs = 32633)

# Configurações de DBSCAN
eps_spatial <- 1000   # Distância máxima espacial em metros
eps_temporal <- 5 * 24   # 5 dias em horas 

# Clustering espacial e temporal sem limitar por município
clustered_data <- fire_data %>%
  mutate(
    time_in_hours = as.numeric(
      difftime(data_pas, min(data_pas), units = "hours")
      ),
    # É necessário dimensionar adequadamente a dimensão temporal para  
    #torná-la compatível com as distâncias espaciais utilizadas no  
    #algoritmo de agrupamento DBSCAN e captar melhor a realidade.
    time_scaled = time_in_hours * (eps_spatial / eps_temporal)
  ) %>%
  group_modify(~ {
    coords <- st_coordinates(.x)
    combined_data <- cbind(coords, .x$time_scaled)
    clusters <- dbscan(combined_data, eps = eps_spatial, minPts = 2)
    .x$cluster_estado <- clusters$cluster
    return(.x)
  }) %>%
  ungroup() %>%
  # Remover clusters de ruído (0)
  filter(cluster_estado > 0) %>%  
  # Reatribuir CRS métrico
  st_as_sf(crs = 32633)          

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
  # Usar geometria existente
  mutate(geometry = st_geometry(cluster_centroid)) %>%  
  # Não redefinir CRS desnecessariamente
  st_as_sf()  

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

# Garantir transformação de filtered_clusters e centroids para EPSG:4326
filtered_clusters <- filtered_clusters %>%
  mutate(
    focus_geometry = st_transform(focus_geometry, crs = 4326),
    centroid_geometry = st_transform(centroid_geometry, crs = 4326),
    bbox_geometry = st_transform(bbox_geometry, crs = 4326)
  )
#-------------------------------------------------------------------------------
# Dataset
unique_clusters <- filtered_clusters %>%
  group_by(cluster_estado) %>%
  summarise(
    centroid_geometry = first(centroid_geometry)  # Use the first centroid
  ) %>%
  ungroup() %>%
  st_as_sf()  # Keep as sf object for spatial operations

# Ensure CRS is metric
unique_clusters <- st_transform(unique_clusters, crs = 32633)

# Extract spatial coordinates for clustering
data <- st_coordinates(unique_clusters$centroid_geometry)

# Extrair coordenadas
coords <- data
# k-distância para 5 vizinhos
kdist <- kNNdist(coords, k = 5)
# Plotar k-distância
plot(sort(kdist), type = "l", main = "Gráfico de k-Distância", xlab = "Pontos Ordenados", ylab = "Distância")
abline(h = 1000, col = "red", lty = 2)  # Linha indicando um eps sugerido

# Define spatial clustering parameters
eps_spatial <- 4000  # Spatial threshold in meters
minPts <- 4           # Minimum points to form a cluster

# Apply DBSCAN for spatial clustering
spatial_clusters <- dbscan(data, eps = eps_spatial, minPts = minPts)

# dataset com superclusters + cluster_estado, etc
unique_clusters <- unique_clusters %>%
  mutate(supercluster_id = spatial_clusters$cluster) %>%
  filter(supercluster_id > 0)  # Remove noise (cluster ID = 0)

# superclusters
superclusters <- unique_clusters %>%
  group_by(supercluster_id) %>%
  summarise(
    supercluster_bbox = st_convex_hull(st_union(centroid_geometry)),  # Use convex hull for the bounding box
    supercluster_centroid = st_centroid(st_union(centroid_geometry)),  # Calculate new centroid
    centroid_count = n()  # Number of centroids in the supercluster
  ) %>%
  st_as_sf()

# Ensure CRS consistency for visualization
superclusters <- superclusters %>%
  mutate(
    supercluster_centroid = st_transform(supercluster_centroid, crs = 4326),
    supercluster_bbox = st_transform(supercluster_bbox, crs = 4326)
  )
unique_clusters <- st_transform(unique_clusters, crs = 4326)

# Compactness (Intra-cluster distances)
supercluster_compactness <- unique_clusters %>%
  group_by(supercluster_id) %>%
  summarise(
    intra_distance = mean(dist(st_coordinates(centroid_geometry))),
    .groups = "drop"
  )

# Separation (Inter-cluster distances)
supercluster_data <- st_coordinates(superclusters$supercluster_centroid)
inter_supercluster_distances <- as.matrix(dist(supercluster_data))

mean_intra_distance <- mean(supercluster_compactness$intra_distance, na.rm = TRUE)
mean_inter_distance <- mean(inter_supercluster_distances[upper.tri(inter_supercluster_distances)])

# Validation Metrics
cat("Average intra-supercluster distance: ", mean_intra_distance, "\n")
cat("Average inter-supercluster distance: ", mean_inter_distance, "\n")

# Interactive map for superclusters
map <- leaflet(superclusters) %>%
  addProviderTiles("CartoDB.Positron") %>%
  # Add bounding boxes for superclusters
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
  # Add supercluster centroids
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
  # Add fire foci cluster centroids
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

# Save the map
saveWidget(map, "Superclusters_with_FireFoci_Clusters.html")

# Add variables from superclusters to unique_clusters using supercluster_id
superclusters_non_geom <- superclusters %>%
  as.data.frame() %>%  # Convert to data frame, dropping geometry
  select(supercluster_id, supercluster_bbox, supercluster_centroid, centroid_count)

regioes_dataset <- unique_clusters %>%
  left_join(superclusters_non_geom, by = "supercluster_id")


# Perform the join using cluster_estado
final_dataset <- filtered_clusters %>%
  left_join(
    regioes_dataset %>%
      st_drop_geometry() %>%  # Drop geometry to simplify the join
      select(cluster_estado, supercluster_id, supercluster_bbox, supercluster_centroid, centroid_count),
    by = "cluster_estado"
  )


