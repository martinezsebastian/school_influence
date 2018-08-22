###### INIT -------------------------------------------------------
###### ------------------------------------------------------------
###### NAME: RCT simulation example - Functions
###### DATE: July 2018
###### Version: 1
###### ------------------------------------------------------------
###### ------------------------------------------------------------
###### https://www.r-bloggers.com/power-analysis-by-simulation-r-rct-malaria-example/
###### COMMENTS:
###### I will define a simple RCT, estimate a the effect of the RCT
###### and then do the same example assuming that there is some 
###### sort of spillover through the network


if(!require(AER)) install.packages("AER",repos = "http://cran.us.r-project.org")
library("AER")
if(!require(paramlink)) install.packages("paramlink",repos = "http://cran.us.r-project.org")
library("paramlink")
if(!require(UsingR)) install.packages("UsingR",repos = "http://cran.us.r-project.org")
library("UsingR")
if(!require(ggplot2)) install.packages("ggplot2",repos = "http://cran.us.r-project.org")
library("ggplot2")
if(!require(GGally)) install.packages("GGally",repos = "http://cran.us.r-project.org")
library("GGally")
if(!require(MASS)) install.packages("MASS",repos = "http://cran.us.r-project.org")
library("MASS")
if(!require(ATE)) install.packages("ATE",repos = "http://cran.us.r-project.org")
library("ATE")
if(!require(igraph)) install.packages("igraph",repos = "http://cran.us.r-project.org")
library("igraph")
if(!require(sna)) install.packages("sna",repos = "http://cran.us.r-project.org")
library("sna")
if(!require(ggnet)) devtools::install_github("briatte/ggnet")
library("ggnet")




  

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#

### Creates a plot with multiple smaller plots inside. I cannot get it to work with the plotlist
### only with the individual plots that come in through ...
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


### I made this function that defines a school
### It makes students and puts them in classrooms
### It gives every student an "attitudes" covariate and a "scores" result
### It also randomises who gets treatment in two ways
### 1. everyone in the school at random, and 2. only in the classrooms selected at random
school <- function(nsim = 100,
                   npop = 100 ,         # Define the sampling population
                   nrooms = 4,        # Defines the number of classrooms in the school
                   crooms = 1/2,        # The proportion of classrooms which get treated 
                   tstudents = 1/2,     # The proportion of students inside each classroom that get treated
                   tscores_m = 0,      # Defines the mean of the distribution of test scores
                   tscores_s = 1,     # Defines the standard deviation of the distribution of test scores
                   ascores_m = 0,      # Defines the mean of the distribution of attitudes
                   ascores_s = 1,     # Defines the standard deviation of the distribution of attitudes
                   corr = 0.7)         # Correlation between attitudes and scores
{
  # Checks that the students can be allocated equally through classrooms
  s_in_r <- npop/nrooms
  if(!(s_in_r%%1==0)==TRUE){print("ERRORSH! Students cannot be divided equally in the defined number of rooms.")}
  
  # Creates a vector with classrooms
  rooms <- vector(mode = "character", length = npop)
  roomnames <- vector(mode = "character", length = nrooms)
  
  # Gives every student a classroom in order
  j <- 1
  for(i in 1:nrooms){
    roomname <- paste0("Room", i)
    rooms[j:(s_in_r*i)] <- roomname
    roomnames[i] <- roomname
    j <- (s_in_r*i) + 1
  }
  
  
  # Gives students attitudes towards education sampling from a random normal distribution 
  # with mu and sigma as parameters
  attitudes <- matrix(rnorm(npop, mean = ascores_m, sd = ascores_s), ncol = 1)
  
  # This whole bit makes the scores vaguely as a function of the students attitudes
  # It can be improved
  mu <- c(tscores_m, ascores_m)
  Sigma <- matrix(corr, nrow=2, ncol=2) + diag(2)*(1-corr)
   
  raw <- mvrnorm(n=npop, mu=mu, Sigma=Sigma)
  gamma <- rgamma(n=npop, shape = 2, scale = 3)
  
  # This makes two kinds of scores, one that includes the attitudes and one that does not
  # I am still to verify if this has an effect
  
  scores <- matrix((attitudes+raw[,1]+gamma), ncol = 1)

  # This makes the treatments, and assigns the treated classrooms
  treatment <- matrix(ifelse(runif(npop)>0.5, "treatment", "control"), ncol = 1)
  treatment_room_ind <- matrix(ifelse(runif(nrooms)>0.5, "treatment room", "control room"), ncol = 1)
  treatment_room <- vector( mode = "character",  length = npop)
  j <- 1
  for(i in roomnames){
    treatment_room[rooms==i] <- treatment_room_ind[j]
    j <- j+1
  }
  treatment_by_room <- treatment
  treatment_by_room[treatment_room=="control room"] <- "control"
  
  # Saves everything in a data frame
  school <- data.frame(scores, attitudes, treatment, rooms, treatment_room, treatment_by_room)
  school$id <- rownames(school)
  # Return
  return(school)
}












