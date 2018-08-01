###### INIT -------------------------------------------------------
###### ------------------------------------------------------------
###### NAME: RCT simulation example - Run
###### DATE: July 2018
###### Version: 2
###### ------------------------------------------------------------
###### ------------------------------------------------------------
###### https://www.r-bloggers.com/power-analysis-by-simulation-r-rct-malaria-example/
###### COMMENTS:
###### I will define a simple RCT, estimate a the effect of the RCT
###### and then do the same example assuming that there is some 
###### sort of spillover through the network


setwd("/Users/sebastianmartinez/Dropbox/0. UoG/Projects/RCT Sim")

source(file = "Students.R")
####### THINGS I NEED TO INCLUDE:
## Treatment does not affect everyone equally
## More Covariates
## I mean, this is a great treatment, and we do see an effect. Make it more complicated


# Make a school with 1000 students
n_students <- 2000
n_classrooms <- 2
schl <- school(npop = n_students, nrooms = n_classrooms)
head(schl)

# color palette
cbbPalette <- c("#56B4E9", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Two simple plots showing the difference in the way that the scores are calculated
q1 <- ggplot(schl[schl$rooms=="Room1",], aes(x = attitudes, y = scores2, color = treatment)) + geom_point()+ geom_smooth() + scale_colour_manual(values=cbbPalette) + ggtitle("Classroom 1")
q2 <- ggplot(schl[schl$rooms=="Room2",], aes(x = attitudes, y = scores2, color = treatment)) + geom_point()+ geom_smooth() +  scale_colour_manual(values=cbbPalette) + ggtitle("Classroom 2")


# Multiplot of the two different scores
#setwd("/Users/sebastianmartinez/Dropbox/0. UoG/1/Posters and Presentations and Drafts/Drafts/Matching/2")
pdf("classrooms.pdf", width = 8, height = 4)
q <- multiplot(q1, q2, cols=2)
dev.off()
# You can see that scores2 has a connection with the attitudes. This makes more sense
#typeof(q)
#ggsave("classrooms.pdf", width = 8, height = 4, plot = q)

############
### THIS DEFINES THE EFFECT OF THE INTERVENTION
############
improvement <- 1.5


# Replicates the school and applies the treatment
t_schl <- schl
conditions <- t_schl$treatment=="treatment"&t_schl$rooms=="Room1"
t_schl$scores2[conditions] <- (t_schl$scores2[conditions])*improvement


# This calculates the Average Treatment Effect
ate1 <- ATE(Y = t_schl$scores2, Ti = ifelse(conditions,1,0), X = as.matrix(t_schl$attitudes))
summary(ate1)

ate2 <- ATE(Y = t_schl$scores2, Ti = ifelse(t_schl$treatment=="treatment",1,0), X = as.matrix(t_schl$attitudes))
summary(ate2)
# I do not know how to read this graph
plot(ate1)


# This bit makes a simple comparison to see visually what was the effect of the treatment on the treated
comparison_pre <- data.frame(schl)
comparison_pre$when <- "pre"
comparison_post <- data.frame(t_schl)
comparison_post$when <- "post"

comparison <- rbind(comparison_pre,comparison_post)

q3 <- ggplot(comparison[comparison$rooms=="Room1",], aes(x = attitudes, y = scores2, color = when, shape = when)) + geom_point() + geom_smooth() + scale_colour_manual(values=cbbPalette) + ggtitle("Classroom 1 (pre and post)")
q4 <- ggplot(comparison[comparison$rooms=="Room2",], aes(x = attitudes, y = scores2, color = when, shape = when)) + geom_point() + geom_smooth() + scale_colour_manual(values=cbbPalette) + ggtitle("Classroom 2 (pre and post)")

pdf("classrooms_prepost.pdf", width = 8, height = 4)
q <- multiplot(q3, q4, cols=2)
dev.off()

############# Now I want to make the network between the students
## Ideally this would be a function that takes a school and returns a school plus the network

# random graph based on classroom 1
class1 <- schl[schl$rooms=="Room1",]
#net <- sna::rgraph(length(class1[,1]),  mode = "graph", tprob = 0.0022)
net <- sna::rgraph(length(class1[,1]),  mode = "graph", tprob = 0.01)
net <- network::network(net, directed = FALSE)
a <- as.matrix(print(net,print.adj = TRUE, matrix.type = "adjacency"))
#View(as.data.frame(a))
network.vertex.names(net) <- class1$id
net$id <- class1$id
net$treat <- (class1$treatment)
# vertex names
# 
pdf("Classroom1_Network.pdf", width = 6, height = 4)
#ggnet2(net, node.size = 1.5, node.color = net$treat, edge.size = 0.5, edge.color = "grey", node.label = net$id)
ggnet2(net, node.size = 1.5, node.color = net$treat, edge.size = 0.5, edge.color = "grey")
dev.off()



inflnc <- data.frame(to_transmit=class1$scores2)
inflnc$id <- class1$id
inflnc$treatment <- class1$treatment
inflnc$scores <- inflnc$to_transmit
inflnc$to_transmit[inflnc$treatment=="control"] <- 0
inflnc$multiplier <- 1/rowSums(a)
inflnc$multiplier[inflnc$treatment=="control" && inflnc$multiplier==Inf] <- 0
inflnc$multiplier[inflnc$multiplier==Inf] <- 1
inflnc$transmission <- a%*%as.matrix(inflnc$to_transmit)%*%0.5 + inflnc$scores
inflnc$transmission2 <- a%*%as.matrix(inflnc$to_transmit)*inflnc$multiplier + inflnc$scores
inflnc$transmission[inflnc$treatment=="treatment"] <- inflnc$scores[inflnc$treatment=="treatment"]
inflnc$transmission2[inflnc$treatment=="treatment"] <- inflnc$scores[inflnc$treatment=="treatment"]
inflnc[794,]


pdf("Interference results.pdf", width = 6, height = 4)
ggplot(as.data.frame(inflnc), aes(x = scores, y = transmission, color = treatment)) + geom_point()  + geom_abline() + ggtitle("Spread of influence") + scale_fill_discrete(guide=FALSE)
ggplot(as.data.frame(inflnc), aes(x = transmission2, y = transmission, color = treatment)) + geom_point()  + geom_abline() + ggtitle("Spread of influence") + scale_fill_discrete(guide=FALSE)
dev.off()

inflnc$transmission[inflnc$transmission<inflnc$transmission2]
inflnc$transmission2[inflnc$transmission<inflnc$transmission2]
0



class1$inter_scores <- inflnc$transmission

# Replicates the school and applies the treatment
inter_t_schl <- class1
inter_t_schl$inter_scores[inter_t_schl$treatment=="treatment"] <- (inter_t_schl$inter_scores[inter_t_schl$treatment=="treatment"])*improvement
inter_t_schl$scores2[inter_t_schl$treatment=="treatment"] <- (inter_t_schl$scores2[inter_t_schl$treatment=="treatment"])*improvement

inter_ate <- ATE(Y = inter_t_schl$inter_scores, Ti = ifelse(inter_t_schl$treatment=="treatment",1,0), X = as.matrix(inter_t_schl$attitudes))
non_inter_ate <- ATE(Y = inter_t_schl$scores2, Ti = ifelse(inter_t_schl$treatment=="treatment",1,0), X = as.matrix(inter_t_schl$attitudes))

summary(inter_ate)
summary(non_inter_ate)



# Resources
# http://kateto.net/netscix2016
# http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually
# THIS ONE https://ggplot2.tidyverse.org/reference/geom_abline.html
# https://briatte.github.io/ggnet/#dependencies
# 