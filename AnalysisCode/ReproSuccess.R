

lh <- read.csv("LifeHistory_20220828.csv")

lh$Birth.Date <- as.Date(lh$Birth.Date)

all_surveys <- read.csv("all_surveys_20211028.csv")

all_surveys$Observation.Date <- as.Date(all_surveys$Observation.Date)

# Select females older than 11 seen between 2001-2011

preMHW <- all_surveys[which(all_surveys$Observation.Date >= "2001-01-01"&
                        all_surveys$Observation.Date <= "2011-01-01"),]

preMHW <- preMHW[which(preMHW$sex == "FEMALE" & preMHW$age >= 11),]

pre_females <- unique(preMHW$Dolphin.ID)

# Select females with 10+ sightings

counts <- table(preMHW$Dolphin.ID)

counts <- counts[which(counts >= 10)]

preMHW <- preMHW[which(preMHW$Dolphin.ID %in% names(counts)), ]

pre_females <- unique(preMHW$Dolphin.ID)

library(igraph)

library (remotes)

remotes :: install_github("vjf2/SocGen")

library(SocGen)

SRI <- simple_ratio (sightings = preMHW,
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

plot(network, labels = FALSE)

deg <- degree(network)

strength <- graph.strength(network)

# Reproductive success (will need to rerun with updated file and sort into pre/post MHW)

repro <- read.csv ("reprosuccess_2009_2019.csv")

repro <- repro[which(repro$gaps == 0), ]

repro$strength <- strength[match(repro$X, names(strength))]

cor(repro$calving_success, repro$strength, use = "complete.obs")

mod <- lm(repro$calving_success~repro$strength)

summary(mod)







