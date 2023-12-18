library(dplyr)
library(igraph)
library(ggplot2)
library(ggrepel)
library(ggraph)

marvel.edge <- read.csv("marvel-edges.csv")
marvel.node <- read.csv("marvel-nodes.csv")

marvel.node %>% head
marvel.edge %>% head

marvel.graph <- 
  graph_from_data_frame(
    d = marvel.edge, 
    vertices = marvel.node, 
    directed = FALSE)
marvel.graph

# Measure Network indices
marvel.graph.tbl <-
  data.frame(
    node = V(marvel.graph) %>% names,
    deg = marvel.graph %>% degree,
    bet = marvel.graph %>% betweenness,
    clo = marvel.graph %>% closeness
  )

# Deg-BTW table
marvel.graph.tbl %>%
  mutate(deg=log(deg+1)/max(log(deg+1)),
         bet=log(bet+1)/max(log(bet+1))) %>%
  ggplot(aes(x=deg, y=bet)) +
  geom_vline(xintercept=0.5, linetype='dashed', color='red') +
  geom_hline(yintercept=0.5, linetype='dashed', color='red') +
  geom_point() + 
  geom_label_repel(aes(label=node)) 

# Network visualization
set.seed(1)
marvel.graph %>%
  ggraph(layout = "auto") +
  geom_edge_arc(colour= "gray50", 
                strength = .2, alpha = .2) +
  geom_node_text(aes(label = name),
                 repel = TRUE,
                 colour="gray10") +
  theme_graph(background = "white") 
