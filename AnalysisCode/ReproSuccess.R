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

#Reproductive success data 

repro <- read.csv("RawData/reprosuccess_2011_2020.csv")

repro <- repro[which(repro$gaps < 3), ]

repro$strength <- strength[match(repro$X, names(strength))]
repro$degree <- deg[match(repro$X, names(strength))]
repro$eig <- eig[match(repro$X, names(strength))]
repro$closeness <- closeness[match(repro$X, names(strength))]

repro <- repro[which(repro$X %in% pre_females),]

cor(repro[,c("degree", "eig", "closeness", "strength")])

cor(repro$survivingcalves, repro$strength, use = "complete.obs")
cor(repro$survivingcalves, repro$degree, use = "complete.obs")
cor(repro$survivingcalves, repro$eig, use = "complete.obs")
cor(repro$survivingcalves, repro$closeness, use = "complete.obs")

repro$birthyear <- lh$Birth.Date[match(repro$X, lh$Dolphin.ID)]
repro$birthyear <- format(repro$birthyear, "%Y") |> as.numeric()

repro$counts <- counts[match(repro$X, names(counts))]

#Add foraging data 


foraging_data <- read.csv("RawData/foraging_categories.csv")

repro$cluster <- foraging_data$ClusterName[match(repro$X, 
                                                 foraging_data$Dolphin.ID)]

#Set TDPDFOR to reference level 

repro$cluster <- factor(repro$cluster, levels = c("TDPDFOR", 
                                                     "MLLFOR/TDPDFOR", 
                                                     "SEAGRASS FOR", 
                                                     "SPF"))

mod <- glm(survivingcalves~closeness + degree + eig + strength + 
             cluster + birthyear + log(counts), 
           data = repro, family = "poisson") 

summary(mod)







