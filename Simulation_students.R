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

if(!require(tidyverse)) install.packages("tidyverse",repos = "http://cran.us.r-project.org")
library("tidyverse")

if(!require(doBy)) install.packages("doBy",repos = "http://cran.us.r-project.org")
library("doBy")

setwd("/Users/sebastianmartinez/Dropbox/0. UoG/Projects/RCT Sim")

source(file = "Students.R")
####### THINGS I NEED TO INCLUDE:
## Treatment does not affect everyone equally
## More Covariates
## I mean, this is a great treatment, and we do see an effect. Make it more complicated

<<<<<<< HEAD

SIMGRID <- list(n_students = seq(from = 25, to = 300, by = 25),
           n_classrooms = 1,
           dens = seq(from = 0.0001, to = 0.01, by = 0.0005),
           improv = 1.8) %>%
  cross_df()
=======
st <- t(matrix(seq(from = 25, to = 500, by = 25), ncol = 5))
dns <- t(matrix(seq(from = 0.0005, to = 0.05, by = 0.0005), ncol = 20))
# Divide the grid into groups of 5X20

out <- vector(mode = "list", length = 5*20)
count <- 1
student_steps <- 2 
density_steps <- 5

for (i in (1:student_steps)) {
  for (j in (1:(density_steps*4))) {
    success <- NULL    
    attempts <- 1 
    while (is.null(success) && attempts <=10){
      attempts <- attempts + 1
      try({
            SIMGRID <- list(n_students = st[i,],
                        n_classrooms = 1,
                        dens = dns[j,],
                        improv = 1.8) %>% cross_df()
              out[[count]] <- sim_results(sim_grid = SIMGRID, SIMULATIONS = 20)
              success <- out[[count]]
              print(attempts)
      })
    }
      count <- count + 1
      print( c(i, j) )
    }
}




to_plot <- data.frame(matrix(ncol = 6 ))
colnames(to_plot) <- (c("n_students", "density", "true_est.mean",  "true_p.mean", "naive_est.mean",  "naive_p.mean"))



for (i in 1:(student_steps*density_steps*4)){
  to_plot <- rbind(extract_to_plot(out[[i]]), to_plot)
}
to_plot <- to_plot[-(nrow(to_plot)),]

# save(out, file = "School_out.RData")
# save(to_plot, file = "School_to_plot.RData")
# load("School_to_plot.RData")
# load("/Users/sebastianmartinez/Dropbox/0. UoG/Projects/BIG FILES/School_out.RData")




cbbPalette <- c("#56B4E9", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbbPalette <- c("#F0E442")
cbbPalette <- c("#D55E00")

## INTERPOL
naive_est_interpol <- ggplot(data = to_plot, aes(x = density, y = n_students)) + 
  geom_raster(aes(fill = naive_est.mean), 
              interpolate = TRUE) +
  scale_fill_gradientn(colours=c("#000000", "#D55E00"), 
                       guide = guide_colorbar(barwidth = 1, 
                                              barheight = 10, 
                                              title = "Mean Estimate", 
                                              ticks = FALSE, 
                                              reverse = TRUE, 
                                              title.theme = element_text(angle = -90), 
                                              title.position = "right", 
                                              title.hjust = 0.5,
                                              draw.ulim = TRUE,
                                              draw.llim = TRUE), 
                       breaks = c(0, 2, 4, 6, 8, 10)) +
  labs(y = "Number of Students", x = "Probability")



## TILEZ
true_est_tiles <- ggplot(data = to_plot, aes(x = density, y = n_students)) + 
  geom_tile(aes(fill = true_est.mean)) +
  scale_fill_gradientn(colours=c("#000000", "#D55E00")) +
  labs(y = "Number of Students", x = "Probability") + 
  guides(fill=guide_legend(title="Mean Estimate"))

## INTERPOL
true_est_interpol <- ggplot(data = to_plot, aes(x = density, y = n_students)) + 
  geom_raster(aes(fill = true_est.mean), 
              interpolate = TRUE) +
  scale_fill_gradientn(colours=c("#000000", "#D55E00"), 
                       guide = guide_colorbar(barwidth = 1, 
                                              barheight = 10, 
                                              title = "Mean Estimate", 
                                              ticks = FALSE, 
                                              reverse = TRUE, 
                                              title.theme = element_text(angle = -90), 
                                              title.position = "right", 
                                              title.hjust = 0.5,
                                              draw.ulim = TRUE,
                                              draw.llim = TRUE), 
                       breaks = c(0, 2, 4, 6, 8, 10)) +
  labs(y = "Number of Students", x = "Probability")

## TILES
true_p_tiles <- ggplot(data = to_plot, aes(x = density, y = n_students)) + 
  geom_tile(aes(fill = true_p.mean)) +
  scale_fill_gradientn(colours=c("#000000", "#D55E00")) +
  labs(y = "Number of Students", x = "Probability") + 
  guides(fill=guide_legend(title="Mean Significance"))

## INTERPOL
true_p_interpol <- ggplot(data = to_plot, aes(x = density, y = n_students)) + 
  geom_raster(aes(fill = true_p.mean), 
              interpolate = TRUE) +
  scale_fill_gradientn(colours=c("#000000", "#D55E00"), 
                       guide = guide_colorbar(barwidth = 1, 
                                              barheight = 10, 
                                              title = "Mean Significance", 
                                              ticks = FALSE, 
                                              reverse = TRUE, 
                                              title.theme = element_text(angle = -90), 
                                              title.position = "right", 
                                              title.hjust = 0.5), 
                       breaks = seq(from = 0, to = 0.55, by = 0.05)) +
  labs(y = "Number of Students", x = "Probability")

## INTERPOL
naive_p_interpol <- ggplot(data = to_plot, aes(x = density, y = n_students)) + 
  geom_raster(aes(fill = naive_p.mean), 
              interpolate = TRUE) +
  scale_fill_gradientn(colours=c("#000000", "#D55E00"), 
                       guide = guide_colorbar(barwidth = 1, 
                                              barheight = 10, 
                                              title = "Mean Significance", 
                                              ticks = FALSE, 
                                              reverse = TRUE, 
                                              title.theme = element_text(angle = -90), 
                                              title.position = "right", 
                                              title.hjust = 0.5), 
                       breaks = seq(from = 0, to = 0.3, by = 0.025)) +
  labs(y = "Number of Students", x = "Probability")




pdf("true_est_tiles.pdf", width = 8, height = 4)
true_est_tiles
dev.off()

pdf("true_p_tiles.pdf", width = 8, height = 4)
true_p_tiles
dev.off()

pdf("true_est_interpol.pdf", width = 8, height = 4)
true_est_interpol
dev.off()

pdf("true_p_interpol.pdf", width = 8, height = 4)
true_p_interpol
dev.off()

pdf("naive_est_interpol.pdf", width = 8, height = 4)
naive_est_interpol
dev.off()

pdf("naive_p_interpol.pdf", width = 8, height = 4)
naive_p_interpol
dev.off()



# Option to make lines around tiles white
# ggplot(data = sum_extraction_true2, aes(x = density, y = n_students, fill = true_est.mean)) + geom_tile(colour="white",size=0.25)


extract_to_plot <- function(to_extract){
  extraction <- sim_extract(to_extract)
  sum_extraction <- doBy::summaryBy(true_est + true_p + naive_est + naive_p ~ n_students + density, data = extraction, FUN = mean)  
  return(sum_extraction)
}
>>>>>>> ab832bb... Outout to latex



sim_results <- function(sim_grid, SIMULATIONS = 1){
  sim_size <- dim(sim_grid)[1]
  sim_results <- vector(mode = "list", length = sim_size*SIMULATIONS)
  for(j in 1:SIMULATIONS){
    for(i in 1:sim_size){
      position <- i+(j-1)*sim_size
      s_temp <- school_simul(n_students = sim_grid$n_students[i],
                             n_classrooms = sim_grid$n_classrooms[i],
                             dens = sim_grid$dens[i],
                             improv = sim_grid$improv[i])
      sim_results[[position]]$simulation <- s_temp
      sim_results[[position]]$params <- c(sim_grid$n_students[i], sim_grid$n_classrooms[i], sim_grid$dens[i], sim_grid$improv[i])
    }
  }
  return(sim_results)
}

sim_results[[161]]

log <- capture.output({
  sim_res <- sim_results(sim_grid = SIMGRID, SIMULATIONS = 20)
})

sim_extract <- function(school_simulation) {
  l <- length(school_simulation)
  
  naiv <- vector(mode = "double", length = l)
  true <- vector(mode = "double", length = l)
  corr <- vector(mode = "double", length = l)
  out_names <- c("n_students", "n_classrooms", "density", "improvement", "naive_est", "naive_p", "true_est", "true_p", "correc_est" , "correc_p")
  out <- matrix(nrow = l, ncol = length(out_names))
  i <- 1
  for(i in 1:l){
    temp_naiv <- as.data.frame(summary(school_simulation[[i]]$simulation$naive_ate)$Estimate)
    temp_true <- as.data.frame(summary(school_simulation[[i]]$simulation$true_ate)$Estimate)
    temp_corr <- as.data.frame(summary(school_simulation[[i]]$simulation$corr_treat)$Estimate)
    
    naiv.est <- temp_naiv$Estimate[3]
    true.est <- temp_true$Estimate[3]
    corr.est <- temp_corr$Estimate[3]
    
    naiv.p <- temp_naiv$p.value[3]
    true.p <- temp_true$p.value[3]
    corr.p <- temp_corr$p.value[3]
    
    params <- school_simulation[[i]]$params
    out[i,] <- c(params, naiv.est, naiv.p, true.est, true.p, corr.est, corr.p) 
  }
  out <- as.data.frame(out)
  colnames(out) <- out_names
  return(out)
}

extraction <- sim_extract(sim_res)
sum_extraction_naive <- doBy::summaryBy(naive_est + naive_p ~ n_students + density, data = extraction, FUN = mean)
ggplot(data = sum_extraction_naive, aes(x = density, y = n_students, fill = naive_est.mean)) + geom_tile() 

sum_extraction_true <- doBy::summaryBy(true_est + true_p ~ n_students + density, data = extraction, FUN = mean)

ggplot(data = sum_extraction_true, aes(x = density, y = n_students, fill = true_est.mean)) + geom_tile() 
ggplot(data = sum_extraction_true, aes(x = density, y = n_students, fill = true_p.mean)) + geom_tile() 
extraction[extraction$n_students==50,]
length(extraction)



school_simul <- function(n_students = 100, 
                         n_classrooms = 1, 
                         improv = 1.8,
                         dens = 0.01
                         )
{
  # Make a school with 1000 students
  schl <- school(npop = n_students, nrooms = n_classrooms)
  ############
  ### THIS DEFINES THE EFFECT OF THE INTERVENTION
  ############
  improvement <- improv
  
  
  # Replicates the school and applies the treatment
  conditions <- schl$treatment=="treatment"&schl$rooms=="Room1"
  schl$naive_new_scores <- schl$scores
  schl$naive_new_scores[conditions] <- (schl$scores[conditions])*improvement
  
  
  # This bit makes a simple comparison to see visually what was the effect of the treatment on the treated
  comparison_pre <- data.frame(schl[,c("attitudes", "scores", "id", "treatment")])
  comparison_pre$when <- "pre"
  comparison_post <- data.frame(schl[,c("attitudes", "naive_new_scores", "id", "treatment")])
  comparison_post$scores <- comparison_post$naive_new_scores
  comparison_post <- data.frame(comparison_post[,c("attitudes", "scores", "id", "treatment")])
  comparison_post$when <- "post"
  comparison <- rbind(comparison_pre,comparison_post)
  rm(comparison_pre, comparison_post)
  
  
  ############# Now I want to make the network between the students
  ## Ideally this would be a function that takes a school and returns a school plus the network
  
  # random graph based on classroom 1
  class1 <- schl[schl$rooms=="Room1",]
  #net <- sna::rgraph(length(class1[,1]),  mode = "graph", tprob = 0.0022)
  net <- sna::rgraph(length(class1[,1]),  mode = "graph", tprob = dens)
  net <- network::network(net, directed = FALSE)
  a <- as.matrix(print(net,print.adj = TRUE, matrix.type = "adjacency"))
  #View(as.data.frame(a))
  network.vertex.names(net) <- class1$id
  net$id <- class1$id
  net$treat <- (class1$treatment)
  net$deg <- sna::degree(net)
  # vertex names
  
  
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
  
  naive_ate <- ATE(Y = class1$naive_new_scores, Ti = ifelse(class1$treatment=="treatment",1,0), X = as.matrix(class1$attitudes))
  true_ate <- ATE(Y = class1$true_new_scores, Ti = ifelse(class1$treatment=="treatment",1,0), X = as.matrix(class1$attitudes))
  correct_treatment <- ATE(Y = class1$true_new_scores, Ti = ifelse(class1$true_treatment=="treatment",1,0), X = as.matrix(class1$attitudes), ATT = TRUE)
  
  out <- vector(mode = "list")
  
  out$naive_ate <- naive_ate
  out$true_ate <- true_ate
  out$corr_treat <- correct_treatment
  out$graph <- net
  return(out)
}





# Resources
# http://kateto.net/netscix2016
# http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually
# THIS ONE https://ggplot2.tidyverse.org/reference/geom_abline.html
# https://briatte.github.io/ggnet/#dependencies
# 