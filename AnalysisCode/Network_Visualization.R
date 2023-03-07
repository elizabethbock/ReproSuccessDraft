# Example network

lh <- read.csv("RawData/LifeHistory_20220828.csv")

lh$Birth.Date <- as.Date(lh$Birth.Date)

all_surveys <- read.csv("RawData/all_surveys_20211028.csv")

all_surveys$Observation.Date <- as.Date(all_surveys$Observation.Date)

# Select females older than 11 seen between 2001-2011

preMHW <- all_surveys[which(all_surveys$Observation.Date >= "2001-01-01" &
                              all_surveys$Observation.Date <= "2011-01-01"), ]

preMHW <- preMHW[which(preMHW$sex == "FEMALE" & preMHW$age >= 11),] 

pre_females <- unique(preMHW$Dolphin.ID)

# Select females with 10 or more sightings 

counts <- table(preMHW$Dolphin.ID)

counts <- counts[which(counts >= 10)]

preMHW <- preMHW[which(preMHW$Dolphin.ID %in% names(counts)), ]

pre_females <- unique(preMHW$Dolphin.ID)

library(igraph)

# remotes::install_github("vjf2/SocGen")

library(SocGen)

SRI <- simple_ratio( sightings = preMHW,
                     group_variable = "Observation.ID",
                     dates = "Observation.Date",
                     IDs = "Dolphin.ID",
                     diag = FALSE,
                     symmetric = TRUE,
                     mask = NULL,
                     assocInd = "SRI")

mhead(SRI)


network <- graph_from_adjacency_matrix(SRI, 
                                       mode = "undirected", 
                                       weighted = TRUE, 
                                       diag = FALSE)

deg <- degree(network)

strength <- graph.strength(network)

eig <- eigen_centrality(network)$vector

closeness <- closeness(network)


windows()
plot(network)

# To visualize nodes by attribute, add those attributes to the graph

V(network)$degree <- degree(network)

V(network)$vsize <- degree(network)/4

plot(network, 
     vertex.size = V(network)$vsize, 
     vertex.label = NA)

# Add edge attribute

E(network)$weight |> head()
E(network)$weight |> max()


E(network)$vweight <- E(network)$weight * 15

plot(network, 
     vertex.size = V(network)$vsize, 
     vertex.label = NA, 
     edge.width = E(network)$vweight, 
     edge.curved = rep(-.4, length(E(network)$vweight)))

# Color by foraging category

foraging_data <- read.csv("RawData/foraging_categories.csv")

V(network)$foraging_cluster <- foraging_data$PAM_Cluster[match(
  V(network)$name, foraging_data$Dolphin.ID
)]

V(network)$color <- ifelse(V(network)$foraging_cluster == 3, "orange", 
                           "green")
                           
V(network)$color[is.na(V(network)$color)] <- "grey"

V(network)$color <- adjustcolor(V(network)$color, alpha.f = 0.5)


plot(network, 
     vertex.size = V(network)$vsize, 
     vertex.label = NA, 
     edge.width = E(network)$vweight, 
     edge.curved = rep(-.4, length(E(network)$vweight)), 
     vertex.color = V(network)$color)

# If you want the nodes to be transparent to each other but not the edges
# You can plot two graphs on top of each other 

set.seed(1) #element of randomness to layout that needs to be kept constant

plot(network, 
     vertex.size = V(network)$vsize, 
     vertex.label = NA, 
     edge.width = E(network)$vweight, 
     edge.curved = rep(-.4, length(E(network)$vweight)), 
     vertex.color = "white")

set.seed(1)

plot(network, 
     add = TRUE,
     vertex.size = V(network)$vsize, 
     vertex.label = NA, 
     edge.color = NA,
     edge.curved = rep(-.4, length(E(network)$vweight)), 
     vertex.color = V(network)$color)

# Remove edges less than 0.05

windows()
hist(E(network)$weight)

E(network)$color <- "grey"

E(network)$color <- ifelse(E(network)$vweight < 0.75, NA, E(network)$color)

#Always write out to pdf first

pdf(file = "TablesFigures/network.pdf")

set.seed(1) #element of randomness to layout that needs to be kept constant

plot(network, 
     vertex.size = V(network)$vsize, 
     vertex.label = NA, 
     edge.color = E(network)$color, 
     edge.width = E(network)$vweight, 
     edge.curved = rep(-.4, length(E(network)$vweight)), 
     vertex.color = "white")

set.seed(1)

plot(network, 
     add = TRUE,
     vertex.size = V(network)$vsize, 
     vertex.label = NA, 
     edge.color = NA,
     edge.curved = rep(-.4, length(E(network)$vweight)), 
     vertex.color = V(network)$color)

dev.off()












