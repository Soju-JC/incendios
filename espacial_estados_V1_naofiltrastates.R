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
saveWidget(map, "State_clusters_marker_clustered.html")

# Categorizar clusters por tamanho
filtered_clusters <- filtered_clusters %>%
  group_by(cluster_estado) %>%
  mutate(
    # Número de focos no cluster
    cluster_size = n(),  
    cluster_size_category = case_when(
      cluster_size < 10 ~ "Pequeno (01-09 focos)",
      cluster_size >= 10 & cluster_size < 25 ~ "Médio (10-24 focos)",
      cluster_size >= 25 ~ "Grande (≥ 25 focos)"
    )
  ) %>%
  ungroup()

# Criar uma paleta para as categorias
palette <- colorFactor(
  # Cores para cada categoria
  palette = c("purple", "blue", "lightblue"),  
  domain = c("Pequeno (01-09 focos)", "Médio (10-24 focos)", "Grande (≥ 25 focos)")
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
saveWidget(map, "State_clusters_by_size.html")


################ Validação ################
# Calcula distancias intra-cluster (compactação)
cluster_compactness <- filtered_clusters %>%
  group_by(cluster_estado) %>%
  summarise(
    intra_distance = mean(dist(st_coordinates(focus_geometry))),
    .groups = "drop"
  )

# Calcula distancias inter-cluster (Separação)
centroids_matrix <- st_coordinates(cluster_data_summary$cluster_centroid)
inter_cluster_distances <- as.matrix(dist(centroids_matrix))

mean_intra_distance <- mean(cluster_compactness$intra_distance)
mean_inter_distance <- mean(
  inter_cluster_distances[upper.tri(inter_cluster_distances)]
  )
cat("Average intra-cluster distance: ", mean_intra_distance, "\n")
cat("Average inter-cluster distance: ", mean_inter_distance, "\n")

# Calcular intervalos temporais dentro de cada cluster
temporal_validation <- filtered_clusters %>%
  group_by(cluster_estado) %>%
  summarise(
    start_time = min(data_pas),
    end_time = max(data_pas),
    time_span = as.numeric(difftime(max(data_pas), min(data_pas), units = "days"))
  )

# Exibir clusters com intervalos de tempo
print(temporal_validation)

ggplot(temporal_validation, aes(x = cluster_estado, y = time_span)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Cluster Time Spans", x = "Cluster ID", y = "Time Span (days)")

#################### Dataset para o tempo até a recidiva #######################
################    Usa ponto médio de intervalo no tempo    ###################
#-------------------------------------------------------------------------------
# Dataset
unique_clusters <- filtered_clusters %>%
  group_by(cluster_estado) %>%
  summarise(
    # Use a primeira linha para o centróide
    centroid_geometry = first(centroid_geometry),
    # Use a primeira linha para bounding box
    bbox_geometry = first(bbox_geometry),
    # Hora de início do cluster
    start_time = first(start_time),  
    # Hora de término do cluster
    end_time = first(end_time),  
    # Duração do cluster
    duration = first(duration),   
    # Número de focos de incêndio
    fire_foci_count = n()                          
  ) %>%
  ungroup() %>%
  # Manter como objeto sf para operações espaciais
  st_as_sf()  

# Calcular pontos médios e durações para centróides
centroid_data <- unique_clusters %>%
  mutate(
    # Calcular características temporais para o intervalo
    #A ancoragem ao start_time mínimo garante que todos os valores de tempo 
    #sejam comparáveis
    start_time_num = as.numeric(
      difftime(start_time, min(start_time), units = "hours")
      ),
    end_time_num = as.numeric(
      difftime(end_time, min(start_time), units = "hours")
      ),
    time_midpoint = (start_time_num + end_time_num) / 2,  
    time_duration = end_time_num - start_time_num         
  )

# Combine recursos espaciais e temporais para agrupamento
centroid_coords <- st_coordinates(centroid_data$centroid_geometry)
combined_features <- cbind(
  centroid_coords,
  centroid_data$time_midpoint,  
  centroid_data$time_duration  
)

# Executar DBSCAN nas features espaço-temporais combinadas
eps_centroids <- 5000  # Threshold espacial para centróides (em metros)
eps_temporal_centroids <- 24  # Temporal threshold em horas
scaling_factor <- eps_centroids / eps_temporal_centroids

# Dimensione o tempo para corresponder às distâncias espaciais
combined_features[, 3] <- combined_features[, 3] * scaling_factor

# Aplicar DBSCAN
centroid_clusters <- dbscan(combined_features, eps = eps_centroids, minPts = 1)

# Adicionar rótulos de cluster aos centróides
centroid_data <- centroid_data %>%
  mutate(
    supercluster_id = centroid_clusters$cluster
  ) %>%
  filter(supercluster_id > 0)  # Remove noise clusters (cluster ID = 0)

# Verificar geometrias inválidas
invalid_geometries <- centroid_data %>%
  filter(!st_is_valid(bbox_geometry))

# Imprimir geometrias inválidas
print(invalid_geometries)

# Corrigir geometrias inválidas
centroid_data <- centroid_data %>%
  mutate(bbox_geometry = st_make_valid(bbox_geometry))

# Simplificar geometrias para remover vértices duplicados
centroid_data <- centroid_data %>%
  mutate(bbox_geometry = st_simplify(bbox_geometry, dTolerance = 0.001))

superclusters <- centroid_data %>%
  group_by(supercluster_id) %>%
  summarise(
    # Calcular uma única caixa delimitadora ou envoltória convexa unificada para o supercluster
    supercluster_bbox = st_convex_hull(st_union(bbox_geometry)),  # Usar envoltória convexa
    supercluster_centroid = st_centroid(st_union(centroid_geometry)),  # Novo centróide
    start_time = min(start_time),  # Tempo inicial mais antigo entre os centróides agrupados
    end_time = max(end_time),      # Tempo final mais recente entre os centróides agrupados
    duration = as.numeric(difftime(max(end_time), min(start_time), units = "hours")),  # Duração em horas
    total_fire_foci = sum(fire_foci_count)  # Total de focos de incêndio em todos os clusters
  ) %>%
  st_as_sf()


# Garantir consistência do CRS
superclusters <- st_transform(superclusters, crs = 4326)

# Garantir que os centróides sejam geometrias do tipo POINT válidas
superclusters <- superclusters %>%
  mutate(
    centroid_point = st_centroid(supercluster_centroid)  # Extrair POINT do centróide
  )

# Distâncias intra-supercluster (compacidade)
supercluster_compactness <- centroid_data %>%
  group_by(supercluster_id) %>%
  summarise(
    intra_distance = mean(dist(st_coordinates(centroid_geometry))),
    .groups = "drop"
  )

# Distâncias inter-supercluster (separação)
supercluster_centroids_matrix <- st_coordinates(superclusters$supercluster_centroid)
inter_supercluster_distances <- as.matrix(dist(supercluster_centroids_matrix))

# Médias das distâncias intra e inter-superclusters
mean_intra_distance <- mean(supercluster_compactness$intra_distance, na.rm = TRUE)
mean_inter_distance <- mean(inter_supercluster_distances[upper.tri(inter_supercluster_distances)])

# Imprimir métricas de validação
cat("Distância média intra-supercluster: ", mean_intra_distance, "\n")
cat("Distância média inter-supercluster: ", mean_inter_distance, "\n")

# Criar o mapa interativo com caixas delimitadoras atualizadas
map <- leaflet(superclusters) %>%
  addProviderTiles("CartoDB.Positron") %>%
  # Adicionar caixas delimitadoras envoltórias convexas
  addPolygons(
    data = superclusters,
    color = "blue",
    weight = 2,
    opacity = 0.7,
    fillOpacity = 0.1,
    label = ~paste("Supercluster ID: ", supercluster_id),
    popup = ~paste(
      "Supercluster ID: ", supercluster_id,
      "<br>Total Fire Foci: ", total_fire_foci,
      "<br>Start Time: ", start_time,
      "<br>End Time: ", end_time,
      "<br>Duration: ", duration, " hours"
    )
  ) %>%
  # Adicionar centróides
  addCircleMarkers(
    data = superclusters,
    lng = ~st_coordinates(supercluster_centroid)[, 1],  # Longitude
    lat = ~st_coordinates(supercluster_centroid)[, 2],  # Latitude
    color = "red",
    radius = 5,
    label = ~paste("Centróide do Supercluster ID: ", supercluster_id),
    popup = ~paste(
      "Supercluster ID: ", supercluster_id,
      "<br>Total Fire Foci: ", total_fire_foci,
      "<br>Centroid: ", st_coordinates(supercluster_centroid)[1], 
      ", ", st_coordinates(supercluster_centroid)[2]
    )
  )

# Salvar o mapa
saveWidget(map, "Superclusters_with_bbox_and_centroids.html")

################################################################################
######################    Considera variação temporal    #######################
###########    Considera a informação intervalar do tempo    ###################
################################################################################
#-------------------------------------------------------------------------------
# Dataset
unique_clusters <- filtered_clusters %>%
  group_by(cluster_estado) %>%
  summarise(
    # Use a primeira linha para o centróide
    centroid_geometry = first(centroid_geometry),
    # Use a primeira linha para bounding box
    bbox_geometry = first(bbox_geometry),
    # Hora de início do cluster
    start_time = first(start_time),  
    # Hora de término do cluster
    end_time = first(end_time),  
    # Duração do cluster
    duration = first(duration),   
    # Número de focos de incêndio
    fire_foci_count = n()                          
  ) %>%
  ungroup() %>%
  # Manter como objeto sf para operações espaciais
  st_as_sf() 

# Garantir que o CRS seja métrico
unique_clusters <- st_transform(unique_clusters, crs = 32633)

# # Extrair coordenadas
# coords <- st_coordinates(unique_clusters$centroid_geometry)
# 
# # k-distância para 10 vizinhos
# kdist <- kNNdist(coords, k = 10)
# 
# # Plotar k-distância
# plot(sort(kdist), type = "l", main = "Gráfico de k-Distância", xlab = "Pontos Ordenados", ylab = "Distância")
# abline(h = 1000, col = "red", lty = 2)  # Linha indicando um eps sugerido

# Definir limites para agrupamento
eps_spatial <- 12000  # Spatial threshold em metros
eps_temporal <- 5 * 24 # Temporal threshold em horas
scaling_factor <- eps_spatial / eps_temporal

# Preparar dados para agrupamento
unique_clusters <- unique_clusters %>%
  mutate(
    start_time_num = as.numeric(difftime(start_time, min(start_time), units = "hours")),
    end_time_num = as.numeric(difftime(end_time, min(start_time), units = "hours"))
  )

data <- cbind(
  st_coordinates(unique_clusters$centroid_geometry),  
  unique_clusters$start_time_num,  
  unique_clusters$end_time_num    
)

custom_distance <- function(data, scaling_factor) {
  n <- nrow(data)
  dist_matrix <- matrix(0, n, n)  # Inicializar uma matriz vazia
  
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      # Distância espacial
      spatial_dist <- sqrt((data[i, 1] - data[j, 1])^2 + (data[i, 2] - data[j, 2])^2)
      
      # Distância temporal (sobreposição de intervalos)
      temporal_dist <- max(0, max(data[i, 3], data[j, 3]) - min(data[i, 4], data[j, 4]))
      
      # Combinar distâncias
      combined_dist <- sqrt(spatial_dist^2 + (temporal_dist * scaling_factor)^2)
      dist_matrix[i, j] <- combined_dist
      dist_matrix[j, i] <- combined_dist
    }
  }
  
  return(as.dist(dist_matrix))  # Converter para objeto de distância
}


# Calcular matriz de distância personalizada
dist_matrix <- custom_distance(data, scaling_factor)

# Aplicar DBSCAN
clusters <- dbscan(as.dist(dist_matrix), eps = eps_spatial, minPts = 1)
# table(clusters$cluster) 

# Adicionar rótulos aos clusters
unique_clusters <- unique_clusters %>%
  mutate(supercluster_id = clusters$cluster) %>%
  filter(supercluster_id > 0)

# Resumir resultados para os superclusters
superclusters <- unique_clusters %>%
  group_by(supercluster_id) %>%
  summarise(
    supercluster_bbox = st_buffer(st_centroid(st_union(centroid_geometry)), dist = 5000),  # Buffer em vez de envoltória convexa
    supercluster_centroid = st_centroid(st_union(centroid_geometry)),
    start_time = min(start_time),
    end_time = max(end_time),
    duration = as.numeric(difftime(max(end_time), min(start_time), units = "hours")),
    total_fire_foci = sum(fire_foci_count)
  ) %>%
  st_as_sf()

# Validação: Compacidade
supercluster_compactness <- unique_clusters %>%
  group_by(supercluster_id) %>%
  summarise(
    intra_distance = mean(dist(st_coordinates(centroid_geometry))),
    .groups = "drop"
  )

# Validação: Separação
supercluster_data <- cbind(
  st_coordinates(superclusters$supercluster_centroid),
  as.numeric(difftime(superclusters$start_time, min(superclusters$start_time), units = "hours")) * scaling_factor,
  as.numeric(difftime(superclusters$end_time, min(superclusters$start_time), units = "hours")) * scaling_factor
)

inter_supercluster_distances <- as.matrix(custom_distance(supercluster_data, scaling_factor))

mean_intra_distance <- mean(supercluster_compactness$intra_distance, na.rm = TRUE)
mean_inter_distance <- mean(inter_supercluster_distances[upper.tri(inter_supercluster_distances)])

cat("Distância média intra-supercluster: ", mean_intra_distance, "\n")
cat("Distância média inter-supercluster: ", mean_inter_distance, "\n")

# Garantir transformação de filtered_clusters e centroids para EPSG:4326
superclusters <- superclusters %>%
  mutate(
    supercluster_centroid = st_transform(supercluster_centroid, crs = 4326),
    supercluster_bbox = st_transform(supercluster_bbox, crs = 4326)
  )
unique_clusters <- st_transform(unique_clusters, crs = 4326)

# Criar um mapa interativo para superclusters
map <- leaflet(superclusters) %>%
  addProviderTiles("CartoDB.Positron") %>%
  # Adicionar caixas delimitadoras dos superclusters
  addPolygons(
    data = superclusters,
    color = "blue",
    weight = 2,
    opacity = 0.7,
    fillOpacity = 0.1,
    label = ~paste("Supercluster ID: ", supercluster_id),
    popup = ~paste(
      "Supercluster ID: ", supercluster_id,
      "<br>Total Fire Foci: ", total_fire_foci,
      "<br>Start Time: ", start_time,
      "<br>End Time: ", end_time,
      "<br>Duration: ", duration, " hours"
    )
  ) %>%
  # Adicionar centróides dos superclusters
  addCircleMarkers(
    data = superclusters,
    lng = ~st_coordinates(supercluster_centroid)[, 1],  # Longitude
    lat = ~st_coordinates(supercluster_centroid)[, 2],  # Latitude
    color = "red",
    radius = 5,
    label = ~paste("Centróide do Supercluster ID: ", supercluster_id),
    popup = ~paste(
      "Supercluster ID: ", supercluster_id,
      "<br>Total Fire Foci: ", total_fire_foci,
      "<br>Centroid: ", st_coordinates(supercluster_centroid)[1], 
      ", ", st_coordinates(supercluster_centroid)[2]
    )
  )

# Salvar o mapa
saveWidget(map, "Superclusters_with_Bounding_Boxes_and_Centroids.html")

# Mapa interativo com agrupamento automático
map <- leaflet(unique_clusters) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addMarkers(
    clusterOptions = markerClusterOptions(),  # Ativar agrupamento de marcadores
    popup = ~paste(
      "Cluster ID: ", cluster_estado, 
      "<br>Fire Foci Count: ", fire_foci_count,
      "<br>Start Time: ", start_time, 
      "<br>End Time: ", end_time, 
      "<br>Duration: ", duration, " hours"
    )
  )

# Salvar o mapa
saveWidget(map, "Superclusters_Automatic_Clustering.html")

superclusters <- superclusters %>%
  mutate(
    supercluster_size_category = case_when(
      total_fire_foci < 10 ~ "Small (01-09 foci)",
      total_fire_foci >= 10 & total_fire_foci < 25 ~ "Medium (10-24 foci)",
      total_fire_foci >= 25 ~ "Large (≥ 25 foci)"
    )
  )

# Criar uma paleta para categorias de tamanho
palette <- colorFactor(
  palette = c("purple", "blue", "lightblue"),
  domain = c("Small (01-09 foci)", "Medium (10-24 foci)", "Large (≥ 25 foci)")
)

# Mapa interativo com categorias de tamanho
map <- leaflet(superclusters) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    data = superclusters,
    color = ~palette(supercluster_size_category),  # Cor por categoria de tamanho
    weight = 2,
    opacity = 0.7,
    fillOpacity = 0.1,
    label = ~paste("Supercluster ID: ", supercluster_id),
    popup = ~paste(
      "Supercluster ID: ", supercluster_id,
      "<br>Size Category: ", supercluster_size_category,
      "<br>Total Fire Foci: ", total_fire_foci,
      "<br>Start Time: ", start_time,
      "<br>End Time: ", end_time,
      "<br>Duration: ", duration, " hours"
    )
  ) %>%
  addLegend(
    position = "topright",
    title = "Supercluster Size Category",
    pal = palette,
    values = ~supercluster_size_category
  )

# Salvar o mapa
saveWidget(map, "Superclusters_Categorized_by_Size.html")
#-------------------------------------------------------------------------------