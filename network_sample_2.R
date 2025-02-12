####### Centrality-Based Radial Network Visualization ####### 
####### using igraph, networkD3 ####### 

# install.packages(c("igraph", "networkD3"))  # Uncomment if not installed
library(igraph)
library(networkD3)
rm(list = ls()) 

#### 1. Friendship Network
students <- paste("Student", 1:20)  
friendships <- data.frame(
  source = c(0, 0, 1, 1, 2, 2, 3, 3, 3, 4, 9, 9, 10, 13, 13, 
             14, 15, 16, 17, 17, 18, 19, 2, 5, 8, 9, 12, 9, 
             9, 10, 10, 15, 15),
  target = c(1, 2, 2, 3, 3, 4, 5, 6, 7, 7, 13, 15, 15, 18, 19, 
             19, 1, 2, 3, 4, 6, 7, 8, 13, 16, 11, 15, 3, 
             6, 8, 12, 7, 16) 
)

# Convert Numeric Indices to Student Names
friendships$source_name <- students[friendships$source + 1]  
friendships$target_name <- students[friendships$target + 1]  

#### 2. Convert to an igraph object
g <- graph_from_data_frame(friendships[, c("source_name", "target_name")], directed = TRUE, vertices = data.frame(name = students))

#### 3. Compute Degree Centrality
nodes <- data.frame(id = 0:(length(students)-1), name = students)
nodes$degree_centrality <- degree(g, mode = "all")

#### 4. Assign Colors Based on Degree Centrality
degree_bins <- seq(min(nodes$degree_centrality, na.rm = TRUE), max(nodes$degree_centrality, na.rm = TRUE), length.out = 6)
nodes$group <- cut(nodes$degree_centrality, 
                   breaks = degree_bins, 
                   labels = c("Very Low Centrality (1)", 
                              "Low Centrality (2)", 
                              "Moderate Centrality (3)", 
                              "High Centrality (4)", 
                              "Very High Centrality (5)"),
                   include.lowest = TRUE)

#### 5. Apply Centrality-based Radial Layout
central_node <- which.max(nodes$degree_centrality) 

# `layout_with_gem()`to place the high-centrality node at the center.
layout_matrix <- layout_with_gem(g)
# Fix the central node at (0,0)
layout_matrix[central_node, ] <- c(0, 0)
# Normalize positions and scale for networkD3
nodes$x <- layout_matrix[,1] * 200  
nodes$y <- layout_matrix[,2] * 200  
# Ensure Source and Target Indices are Numeric
friendships$source <- match(friendships$source_name, nodes$name) - 1
friendships$target <- match(friendships$target_name, nodes$name) - 1
friendships$value <- 1  

#### 6. Define Color Mapping for Centrality (Legend)
color_mapping <- 'd3.scaleOrdinal().domain([
                    "Very Low Centrality (1)", 
                    "Low Centrality (2)", 
                    "Moderate Centrality (3)", 
                    "High Centrality (4)", 
                    "Very High Centrality (5)"
                 ])
                 .range(["#ADD8E6", "#32CD32", "#FFA500", "#8B0000", "#000000"]);'

#### 7. Force Simulation
nodes$fx <- nodes$x  
nodes$fy <- nodes$y  

network <- forceNetwork(
  Links = friendships, Nodes = nodes, Source = "source", Target = "target",
  Value = "value", NodeID = "name", Group = "group",
  opacity = 1, zoom = TRUE, legend = TRUE,
  fontSize = 14, arrows = FALSE, 
  linkDistance = 100,  # Control link distance for better spacing
  charge = -50,  # Slight repulsion to avoid too much overlap
  colourScale = JS(color_mapping)
)

#### 8. Save the network visualization as an HTML file
saveNetwork(network, file = "student_friendship_network_Radial_Fixed.html", selfcontained = TRUE)
print(">> Network was saved: please open: student_friendship_network_Radial_Fixed.html")
