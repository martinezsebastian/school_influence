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

if(!require(xtable)) install.packages("xtable",repos = "http://cran.us.r-project.org")
library("xtable")
<<<<<<< HEAD
=======

if(!require(ddpcr)) install.packages("ddpcr",repos = "http://cran.us.r-project.org")
library("ddpcr")

>>>>>>> ab832bb... Outout to latex

setwd("/Users/sebastianmartinez/Dropbox/0. UoG/Projects/RCT Sim")

source(file = "Students.R")
####### THINGS I NEED TO INCLUDE:
## Treatment does not affect everyone equally
## More Covariates
## I mean, this is a great treatment, and we do see an effect. Make it more complicated


# Make a school with 1000 students
n_students <- 100
n_classrooms <- 1
schl <- school(npop = n_students, nrooms = n_classrooms)
head(schl)

# color palette
cbbPalette <- c("#56B4E9", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Two simple plots showing the difference in the way that the scores are calculated
q1 <- ggplot(schl[schl$rooms=="Room1",], aes(x = attitudes, y = scores, color = treatment)) + geom_point()+ geom_smooth() + scale_colour_manual(values=cbbPalette) + labs(y = "Student scores", x = "Attitude towards education")
q2 <- ggplot(schl[schl$rooms=="Room2",], aes(x = attitudes, y = scores2, color = treatment)) + geom_point()+ geom_smooth() +  scale_colour_manual(values=cbbPalette) + ggtitle("Classroom 2")

<<<<<<< HEAD
setwd("/Users/sebastianmartinez/Dropbox/0. UoG/1/Posters and Presentations and Drafts/SPHSU Conference")
pdf("classrooms.pdf", width = 8, height = 4)
q1
dev.off()
=======





#################### UNCOMMENT ####################
q1 <- ggplot(schl[schl$rooms=="Room1",], aes(x = attitudes, y = scores, color = treatment)) + geom_point()+ geom_smooth() + scale_colour_manual(values=cbbPalette) + labs(y = "Student scores", x = "Attitude towards education")
q2 <- ggplot(schl[schl$rooms=="Room2",], aes(x = attitudes, y = scores2, color = treatment)) + geom_point()+ geom_smooth() +  scale_colour_manual(values=cbbPalette) + ggtitle("Classroom 2")

#setwd("/Users/sebastianmartinez/Dropbox/0. UoG/1/Posters and Presentations and Drafts/SPHSU Conference")
pdf("classrooms.pdf", width = 8, height = 4)
q1
dev.off()
#################### UNCOMMENT ####################





>>>>>>> ab832bb... Outout to latex

# Multiplot of the two different scores
#setwd("/Users/sebastianmartinez/Dropbox/0. UoG/1/Posters and Presentations and Drafts/Drafts/Matching/2")
#pdf("classrooms.pdf", width = 8, height = 4)
#q <- multiplot(q1, q2, cols=2)
#dev.off()
# You can see that scores2 has a connection with the attitudes. This makes more sense
#typeof(q)
#ggsave("classrooms.pdf", width = 8, height = 4, plot = q)

############
### THIS DEFINES THE EFFECT OF THE INTERVENTION
############
improvement <- 1.8


# Replicates the school and applies the treatment
conditions <- schl$treatment=="treatment"&schl$rooms=="Room1"
schl$naive_new_scores <- schl$scores
schl$naive_new_scores[conditions] <- (schl$scores[conditions])*improvement


# This calculates the Average Treatment Effect
ate1 <- ATE(Y = schl$naive_new_scores, Ti = ifelse(conditions,1,0), X = as.matrix(schl$attitudes))
summary(ate1)
# I do not know how to read this graph
#plot(ate1)


# This bit makes a simple comparison to see visually what was the effect of the treatment on the treated
comparison_pre <- data.frame(schl[,c("attitudes", "scores", "id", "treatment")])
comparison_pre$when <- "pre"
comparison_post <- data.frame(schl[,c("attitudes", "naive_new_scores", "id", "treatment")])
comparison_post$scores <- comparison_post$naive_new_scores
comparison_post <- data.frame(comparison_post[,c("attitudes", "scores", "id", "treatment")])
comparison_post$when <- "post"
comparison <- rbind(comparison_pre,comparison_post)
rm(comparison_pre, comparison_post)

q3 <- ggplot(comparison[comparison$treatment == "treatment",], aes(x = attitudes, y = scores, shape = when)) + geom_point() + geom_smooth(aes(x = attitudes, y = scores, color = when)) + scale_colour_manual(values=cbbPalette) + labs(y = "Student scores", x = "Attitude towards education")

<<<<<<< HEAD
q4 <- ggplot(comparison[comparison$treatment == "control",], aes(x = attitudes, y = scores, shape = when)) + geom_point() + geom_smooth(aes(x = attitudes, y = scores, color = when)) + scale_colour_manual(values=cbbPalette) + labs(y = "Student scores", x = "Attitude towards education")

=======
#################### UNCOMMENT ####################
q3 <- ggplot(comparison[comparison$treatment == "treatment",], aes(x = attitudes, y = scores, shape = when)) + geom_point() + geom_smooth(aes(x = attitudes, y = scores, color = when)) + scale_colour_manual(values=cbbPalette) + labs(y = "Student scores", x = "Attitude towards education")

q4 <- ggplot(comparison[comparison$treatment == "control",], aes(x = attitudes, y = scores, shape = when)) + geom_point() + geom_smooth(aes(x = attitudes, y = scores, color = when)) + scale_colour_manual(values=cbbPalette) + labs(y = "Student scores", x = "Attitude towards education")
>>>>>>> ab832bb... Outout to latex

pdf("treatmentprepost.pdf", width = 8, height = 4)
q3
dev.off()

<<<<<<< HEAD
pdf("controlprepost.pdf", width = 8, height = 4)
q4
dev.off()

=======
pdf("treatmentprepost.pdf", width = 8, height = 4)
q3
dev.off()

pdf("controlprepost.pdf", width = 8, height = 4)
q4
dev.off()
#################### UNCOMMENT ####################
>>>>>>> ab832bb... Outout to latex

#pdf("classrooms_prepost.pdf", width = 8, height = 4)
#q <- multiplot(q3, q4, cols=2)
#dev.off()

############# Now I want to make the network between the students
## Ideally this would be a function that takes a school and returns a school plus the network

# random graph based on classroom 1
class1 <- schl[schl$rooms=="Room1",]
#net <- sna::rgraph(length(class1[,1]),  mode = "graph", tprob = 0.0022)
net <- sna::rgraph(length(class1[,1]),  mode = "graph", tprob = (1/n_students)*5)
<<<<<<< HEAD
=======

>>>>>>> ab832bb... Outout to latex
net <- network::network(net, directed = FALSE)
a <- as.matrix(print(net,print.adj = TRUE, matrix.type = "adjacency"))
#View(as.data.frame(a))
network.vertex.names(net) <- class1$id
net$id <- class1$id
net$treat <- (class1$treatment)
net$deg <- sna::degree(net)
# vertex names



<<<<<<< HEAD
col = c("control" = cbbPalette[1], "treatment" = cbbPalette[2])
pdf("Classroom_Network.pdf", width = 6, height = 4)
#ggnet2(net, node.size = 1.5, node.color = net$treat, edge.size = 0.5, edge.color = "grey", node.label = net$id)
  ggnet2(net, node.size = 2, node.color = net$treat, edge.size = 0.5, edge.color = "gray", palette = col)
dev.off()
=======

#################### UNCOMMENT ####################
col = c("control" = cbbPalette[1], "treatment" = cbbPalette[2])
pdf("Classroom_Network.pdf", width = 6, height = 4)
# ggnet2(net, node.size = 1.5, node.color = net$treat, edge.size = 0.5, edge.color = "grey", node.label = net$id)
ggnet2(net, node.size = 2, node.color = net$treat, edge.size = 0.5, edge.color = "gray", palette = col)
dev.off()
#################### UNCOMMENT ####################



>>>>>>> ab832bb... Outout to latex

## THIS SECTION CREATES THE LEARNING MULTIPLIER
inflnc <- data.frame(to_transmit = class1$scores)
inflnc$id <- class1$id
inflnc$treatment <- class1$treatment
inflnc$scores <- inflnc$to_transmit
inflnc$degree <- sna::degree(net, gmode = "graph")
inflnc$to_transmit[inflnc$treatment=="control"] <- 0
inflnc$multiplier <- 1/inflnc$degree
inflnc$multiplier[inflnc$multiplier==Inf]  <- 0

inflnc$transmission <- a%*%as.matrix(inflnc$to_transmit)*inflnc$multiplier + inflnc$scores
inflnc$transmission[inflnc$treatment=="treatment"] <- inflnc$scores[inflnc$treatment=="treatment"]
inflnc$learning <- inflnc$transmission/inflnc$scores
inflnc$learning <- ifelse(inflnc$learning > 1, 2, inflnc$learning)
inflnc$influenced <- inflnc$scores * inflnc$learning



<<<<<<< HEAD
pdf("Interference results.pdf", width = 6, height = 4)

ggplot(as.data.frame(inflnc), aes(x = scores, y = influenced, color = treatment)) + geom_point() + scale_fill_discrete(guide=FALSE) + 
  #  geom_text(aes(label=id),hjust=0, vjust=0) + 
=======
#################### UNCOMMENT ####################
pdf("Interference results.pdf", width = 6, height = 4)

ggplot(as.data.frame(inflnc), aes(x = scores, y = influenced, color = treatment)) + geom_point() + scale_fill_discrete(guide=FALSE) + 
  ## This line plots the points with id on the label. Do not need
  # geom_text(aes(label=id),hjust=0, vjust=0) + 
>>>>>>> ab832bb... Outout to latex
  xlim(0,25) + ylim(0,25) +
  geom_abline() +
  geom_abline(slope = 2) +
  labs(title = "Spread of influence", subtitle = "Learning multiplier", y = "Influenced scores", x = "Original scores")
dev.off()
<<<<<<< HEAD
=======
#################### UNCOMMENT ####################


>>>>>>> ab832bb... Outout to latex


class1$learning <- inflnc$learning

# Replicates the school and applies the treatment
# Creating the new scores
class1$true_new_scores <- class1$scores
# The true new scores for the treated units is just the improvement
class1$true_new_scores[class1$treatment=="treatment"] <- (class1$scores[class1$treatment=="treatment"])*improvement
# The true new scores for the control units is at most the total improvement
class1$true_new_scores[class1$treatment=="control"] <- (class1$scores[class1$treatment=="control"])*(improvement*(class1$learning[class1$treatment=="control"]-1))

class1$true_treatment <- "control"
class1$true_treatment <- ifelse(class1$treatment=="treatment", "treatment", "control")
class1$true_treatment <- ifelse(class1$treatment=="control" & (class1$learning-1)==0, "control", "treatment")

non_inter_ate <- ATE(Y = class1$naive_new_scores, Ti = ifelse(class1$treatment=="treatment",1,0), X = as.matrix(class1$attitudes))
inter_ate <- ATE(Y = class1$true_new_scores, Ti = ifelse(class1$treatment=="treatment",1,0), X = as.matrix(class1$attitudes))
<<<<<<< HEAD
correct_treatment <- ATE(Y = class1$true_new_scores, Ti = ifelse(class1$true_treatment=="treatment",1,0), X = as.matrix(class1$attitudes), ATT = TRUE)
=======
>>>>>>> ab832bb... Outout to latex


out_non_inter_ate <- summary(non_inter_ate)
out_inter_ate <- summary(inter_ate)
<<<<<<< HEAD
out_corr_treat <- summary(correct_treatment)

print(xtable(out_inter_ate$Estimate, type = "latex"), file = "interate.tex")
print(xtable(out_non_inter_ate$Estimate, type = "latex"), file = "noninterate.tex")
print(xtable(out_corr_treat$Estimate, type = "latex"), file = "correct.tex")
=======


print(xtable(out_non_inter_ate$Estimate, type = "latex", caption = "Naive ATE", label = "naive_ate"), file = "noninterate.tex")
print(xtable(out_inter_ate$Estimate, type = "latex", caption = "Skeptical ATE", label = "skep_ate"), file = "interate.tex")

>>>>>>> ab832bb... Outout to latex

summary(non_inter_ate)
summary(inter_ate)

ggnet2(net, node.size = 2, node.color = net$treat, edge.size = 0.5, edge.color = "gray", palette = col)



# Resources
# http://kateto.net/netscix2016
# http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually
# THIS ONE https://ggplot2.tidyverse.org/reference/geom_abline.html
# https://briatte.github.io/ggnet/#dependencies
# 