###### ------------------------------------------------------------
###### ------------------------------------------------------------
###### NAME: Horwitz-Thompson and Hajek estimators
###### DATE: September 2018
###### Version: 1
###### ------------------------------------------------------------
###### ------------------------------------------------------------
###### Script to understand the HT and H estimators to include them in the school simulation
###### Following: https://cran.r-project.org/web/packages/sampling/vignettes/HT_Hajek_estimators.pdf

if(!require(sampling)) install.packages("sampling",repos = "http://cran.us.r-project.org")
library("sampling")

data("belgianmunicipalities")
attach(belgianmunicipalities)

# Define sample size
n <- 20
# Define inclusion probabilities
# Tot04 contains the Belgian population on the 1st of July of 2004
# The probability of inclusion is directly related to the number of inhabitants for each municipalitys
pik <- inclusionprobabilities(Tot04, n)

# Now we look at the number of simulations
sim <- 10
ss <- ss1 <- array(0, c(sim, 4))

# Case 1
cat("Case 1\n")
y1 <- rep(3,N)
?cat
# Case 2
cat("Case 2\n")
y2=TaxableIncome

# Case 3
cat("Case 3\n")
x=1:N
pik3=inclusionprobabilities(x,n)
y3=1/pik3

# Case 4
cat("Case 4\n")
epsilon=rnorm(N,0,sqrt(1/3))
pik4=pik3
y4=5*(x+epsilon)

