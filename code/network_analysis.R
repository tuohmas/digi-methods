# Tuomas Heikkilä, 5 Feb, 2024
#
# Introduction to (social) network analysis

# Clean the environment
rm(list = ls())

# Suppress scientific notation
options(scipen = 999)

# Load packages
pacman::p_load(tidyverse,     # For data manipulation
               igraph,        # For network analysis
               RColorBrewer) # For additional color palettes

# load nodes and edges
nodes <- read_csv("data/interest-groups-pseudo-nodes.csv")
edges <- read_csv("data/interest-groups-pseudo-edges.csv")

# create a graph object from a data frame
g <- igraph::graph_from_data_frame(edges, directed = FALSE, vertices = nodes)

# Show graph object
g

# global properties: node and edge counts, density
g_global <-
  data.frame(
    nodes               = vcount(g),
    edges               = ecount(g),
    density             = edge_density(g),
    avg_degree          = mean(degree(g, mode = "total")),
    avg_clustering_coef = transitivity(g, type = "average"),
    components          = count_components(g),
    nodes_in_giant_comp = vcount(largest_component(g)),
    diameter            = diameter(g),
    avg_path_length     = mean_distance(g, directed = FALSE,
                                        weights = E(g)$weight))

glimpse(g_global)

# calculate degree for vertices (nodes), add as an vertex attribute
V(g)$degree <- degree(g, mode = "total")

# Plot degree distribution as an histogram
hist(degree.distribution(g))

hist(table(V(g)$degree), breaks = length(unique(V(g)$degree)),
     main = "degree distributions", xlab = "Degree number")

hist(table(V(g)$degree), breaks = "Freedman-Diaconis",
     main = "degree distributions", xlab = "Degree number")

# Calculate other local properties
V(g)$eigenvector <- eigen_centrality(g, directed = FALSE)$vector
V(g)$pagerank <- page_rank(g, directed = FALSE)$vector
V(g)$betweenness <- betweenness(g, directed = FALSE, normalized = TRUE)
V(g)$core_number <- coreness(g)

# Make a dataframe to compare local properties
g_local <-
  data.frame(node = V(g)$name,
             degree = V(g)$degree,
             betweenness = V(g)$betweenness,
             eigenvector_centr = V(g)$eigenvector,
             pagerank = V(g)$pagerank,
             k_core = V(g)$core_number)

g_local %>% arrange(desc(degree)) %>% head(10)
g_local %>% arrange(desc(degree)) %>% tail(10)

g_local %>% arrange(desc(eigenvector_centr)) %>% head(10)
g_local %>% arrange(desc(eigenvector_centr)) %>% tail(10)

# Plot graph
plot(vertex.color = "light blue", vertex.size = 4,
     edge.color = "grey90", vertex.label = NA,
     edge.width = E(g)$weight)

# Plot with force-directed algorithm
plot(g, vertex.color = "light blue", vertex.size = 4,
     layout = layout_with_fr(g, niter = 1000),
     edge.color = "grey90", vertex.label = NA,
     edge.width = E(g)$weight)
