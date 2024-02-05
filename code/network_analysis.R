# Tuomas Heikkilä, 5 Feb, 2024
#
# Introduction to (social) network analysis

# Clean the environment
rm(list = ls())

# Load packages
pacman::p_load(tidyverse,
               igraph,
               tidygraph,
               RColorBrewer)

# Create random graphs for demonstration purposes
# Each graph has 25 vertices (nodes) randomly connected via 42 edges (ties)
g1 <- igraph::erdos.renyi.game(25, 42, type = "gnm")
g2 <- igraph::erdos.renyi.game(25, 42, type = "gnm")
g3 <- igraph::erdos.renyi.game(25, 42, type = "gnm")
g4 <- igraph::erdos.renyi.game(25, 42, type = "gnm")
g5 <- igraph::erdos.renyi.game(25, 42, type = "gnm")
g6 <- igraph::erdos.renyi.game(25, 42, type = "gnm")
g7 <- igraph::erdos.renyi.game(25, 42, type = "gnm")
g8 <- igraph::erdos.renyi.game(25, 42, type = "gnm")
g9 <- igraph::erdos.renyi.game(25, 42, type = "gnm")

# Lets select one graph object at a time
g <- g3

# Calculate degrees per node; store the value as vertex attribute
V(g)$degree <- degree(g, mode = "all")

# Color graph; edges are black except for one random edge (index 1)
E(g)$color <- "black"
E(g)[1]$color <- RColorBrewer::brewer.pal(8, "PuBu")[6]

# Get nodes (vertex ids) connected via the selected edge
vids <- ends(g, 1) %>% as.numeric()

# Color nodes white except for special pair of nodes
V(g)$color <- "white"
V(g)[vids]$color <- RColorBrewer::brewer.pal(8, "PuBu")[4]

# Size nodes by their degree number, min 2
V(g)$size <- 2 + V(g)$degree

# Plot graph
plot(g, layout = layout_with_kk,
     # Plot network without labels; specially thick edge for special nodes
     vertex.label = NA, edge.width = ifelse(E(g) == 1, 6, 1))
