# data <- read.csv("focos_mensal_br_202409.csv")
# 
# write.table(teste2, "vni_focos.txt",sep = ";", row.names = FALSE, col.names = TRUE)
# library(basedosdados)
# 
# # Defina o seu projeto no Google Cloud
# basedosdados::set_billing_id("projetoincendios")
# 
# # Para carregar o dado direto no R
# query <- "
# SELECT
#     dados.ano as ano,
#     dados.sigla_uf as sigla_uf,
#     dados.id_municipio AS id_municipio,
#     diretorio_id_municipio.nome AS id_municipio_nome,
#     dados.bioma as bioma,
#     dados.id_bdq as id_bdq,
#     dados.id_foco as id_foco,
#     dados.data_hora as data_hora,
#     dados.centroide as centroide
# FROM `basedosdados.br_inpe_queimadas.microdados` AS dados
# LEFT JOIN (SELECT DISTINCT id_municipio,nome  FROM `basedosdados.br_bd_diretorios_brasil.municipio`) AS diretorio_id_municipio
#     ON dados.id_municipio = diretorio_id_municipio.id_municipio
# "
# 
# df <- read_sql(query, billing_project_id = get_billing_id())

# write.csv(df, "basedosdados_queimadas2003_2022.csv", row.names = FALSE)


# df <- read.csv("basedosdados_queimadas2003_2022.csv", header = TRUE)
# df2 <- subset(df, df$ano == 2022 & df$sigla_uf == "ES")

df_inpe <- read.csv("focos_br_todos-sats_2023.csv", header = TRUE)
df_inpe2 <- subset(df_inpe, df_inpe$estado == "ESPÍRITO SANTO")

library(tidyverse)
library(sf)
library(dbscan)

# # Convert data to an sf object
# fire_data <- st_as_sf(df_inpe2, coords = c("longitude", "latitude"), crs = 4326)
# fire_data <- st_transform(fire_data, crs = 32633) # convert to UTM for metric distances
# 
# # Adding a time variable in a format compatible with dbscan
# fire_data$time <- as.numeric(difftime(fire_data$data_pas, min(fire_data$data_pas), units = "mins"))
# 
# # Define spatial and temporal epsilon values
# spatial_eps <- 500 # meters
# temporal_eps <- 60 # minutes
# 
# # Perform dbscan on combined spatial and temporal data
# combined_data <- cbind(st_coordinates(fire_data), fire_data$time)
# clusters <- dbscan(combined_data, eps = c(spatial_eps, temporal_eps), minPts = 5) # minPts defines cluster density
# 
# # Add cluster labels to the original data
# fire_data$cluster <- clusters$cluster

# Convert data to an sf object and transform to UTM for spatial distances
fire_data <- st_as_sf(df_inpe2, coords = c("longitude", "latitude"), crs = 4326)
fire_data <- st_transform(fire_data, crs = 32633) # Convert to UTM

# Extract coordinates in meters and normalize spatial coordinates
coords <- st_coordinates(fire_data)
coords <- scale(coords)

# Normalize the time variable and combine with spatial data
fire_data$time <- as.numeric(difftime(fire_data$data_pas, min(fire_data$data_pas), units = "mins"))
time_scaled <- scale(fire_data$time)

# Combine scaled spatial and temporal data for clustering
combined_data <- cbind(coords, time_scaled)

# Set a single eps for the combined scaled data
eps_value <- 0.5 # Adjust as needed considering data scale
clusters <- dbscan(combined_data, eps = eps_value, minPts =)

# Add cluster labels to the original data
fire_data$cluster <- clusters$cluster


#------------------ Considering the cities boundaries -------------------------

# Convert to sf and transform to a metric CRS, if not already done
fire_data <- st_as_sf(df_inpe2, coords = c("longitude", "latitude"), crs = 4326)
fire_data <- st_transform(fire_data, crs = 32633) # Change CRS as needed

# Apply clustering within each municipality
clustered_data <- fire_data %>%
  group_by(municipio) %>%
  group_modify(~ {
    # Extract coordinates and normalize time
    coords <- st_coordinates(.x)
    time_scaled <- scale(as.numeric(difftime(.x$data_pas, min(.x$data_pas), units = "mins")))
    
    # Combine spatial and temporal dimensions
    combined_data <- cbind(coords, time_scaled)
    
    # Run dbscan on the combined data
    eps_value <- 0.5 # Adjust based on your scale and data characteristics
    minPts <- 3
    clusters <- dbscan(combined_data, eps = eps_value, minPts = minPts)
    
    # Add cluster results to the subset data
    .x$cluster <- clusters$cluster
    return(.x)
  }) %>%
  ungroup()

# Convert to sf, ensuring the geometry column is kept as is
clustered_data_sf <- st_sf(clustered_data, geometry = clustered_data$geometry)

# Check CRS
st_crs(clustered_data_sf)

# # Set CRS if it's missing
# if (is.na(st_crs(clustered_data_sf))) {
#   st_crs(clustered_data_sf) <- 4326  # WGS 84
# }

# ggplot(clustered_data_sf) +
#   geom_sf(aes(color = as.factor(cluster)), size = 2, alpha = 0.7) +
#   labs(color = "Cluster ID") +
#   theme_minimal() +
#   facet_wrap(~ municipio) + # Separate plot by municipality if desired
#   ggtitle("Fire Clusters by Municipality") +
#   theme(legend.position = "right")

# Assuming a shapefile of municipalities 
municipios <- st_read("ES_Municipios_2022.shp")

ggplot() +
  geom_sf(data = municipios, fill = "white", color = "black") +
  geom_sf(data = clustered_data_sf, aes(color = as.factor(cluster)), size = 2, alpha = 0.7) +
  labs(color = "Cluster ID") +
  theme_minimal() +
  ggtitle("Fire Clusters with Municipal Boundaries")

# Interactive Plot

library(leaflet)
library(RColorBrewer)

# Generate a color palette for 41 clusters
num_clusters <- length(unique(clustered_data_sf$cluster))  # Get the number of unique clusters

# Generate enough colors (use Set3, but if you have more than 12 clusters, consider another approach)
if (num_clusters <= 12) {
  colors <- brewer.pal(num_clusters, "Set3")  # Use Set3 for up to 12 clusters
} else {
  # For more than 12 clusters, use a rainbow palette or another method
  colors <- colorRampPalette(brewer.pal(9, "Set3"))(num_clusters)  # Create more colors
}

# Create a vector of labels corresponding to each cluster
labels <- paste("Cluster", 1:num_clusters)  # Dynamically label the clusters

# Check the CRS
st_crs(clustered_data_sf)

# Transform to EPSG:4326 (WGS 84)
clustered_data_sf <- st_transform(clustered_data_sf, crs = 4326)

library(htmlwidgets)

map <- leaflet(clustered_data_sf) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(
    color = ~factor(cluster),  
    radius = 5,                
    opacity = 0.7,             
    fillOpacity = 0.7,         
    popup = ~paste("Cluster: ", cluster, "<br>Municipality: ", municipio)
  ) %>%
  addLegend(
    position = "topright",
    title = "Cluster ID",
    colors = colors,  
    labels = labels   
  )

# Save the map to an HTML file
saveWidget(map, "fire_clusters_map.html")
