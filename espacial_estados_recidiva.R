library(tidyverse)
library(sf) # Objeto de coordenadas 
library(dbscan) # Clusterização
library(lubridate)
library(leaflet) # Plot
library(leaflet.extras) # Plot
library(htmlwidgets) # Plot

# Carregar e filtrar os dados
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
#-------------------------------------------------------------------------------

# Clustering espacial e temporal sem limitar por município
clustered_data <- filtered_clusters %>%
  mutate(
    time_in_hours = as.numeric(difftime(data_pas, min(data_pas), units = "hours")),
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
  filter(cluster_estado > 0) %>%  # Remover clusters de ruído (0)
  st_as_sf(crs = 32633)          # Reatribuir CRS métrico
