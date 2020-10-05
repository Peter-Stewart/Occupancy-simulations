# README ####
# In this version the simulation runs only once
# The purpose of this simulation is to act as core for multiple loop simulations
# and also to try out different covariate structures etc. 

# Packages ####
library(igraph)
library(boot)
library(dplyr)
library(unmarked)

# Study design ####
sites <- 3000 # Number of sites surveyed
K <- 40 # Number of times each site is surveyed (e.g. no. camera nights)
det.prob <- 0.5 # Probability of detection does not depend on any covariates in this model

# Covariate dependence structure ####
fig1 <- graph.formula(X -+ Psi, Z -+ X, Z -+ Psi, 
                      Q -+ P, R -+ P,
                      simplify = FALSE)

# True effect sizes (beta values and intercept) NB. these will be on logit scale ####
# For occupancy
alpha <- 0.5 # Intercept value for psi
B1 <- 0.3 # Effect of X on Psi
B2 <- 0.8 # Effect of Z on X
B3 <- -0.4 # Effect of Z on Psi

# For detection
B4 <- 0.4 # Effect of R on P
B5 <- -0.2 # Effect of Q on P
  
# Explanatory variable values ####
# For occupancy
Z <- rnorm(sites,4,1)

# For detection
R <- rnorm(sites,2,1)
Q <- rnorm(sites,4,1)

# Response variable values other than psi and p ####
X <- 0 + (B2*Z) + rnorm(sites,0,0.1)

# True psi value ####
logit_psi_t <- alpha + (B1*X) + (B3*Z) 
psi_t <- inv.logit(logit_psi_t)

# True probability of detection (p) ####
logit.det.prob <- 0.5 + (B4*R) + (B5*Q)
det.prob <- inv.logit(logit.det.prob)

# Bind Psi and covariates into a "landscape" (dataframe) ####
grid <- cbind(psi_t, logit_psi_t, X, Z, det.prob, logit.det.prob, R, Q)
grid <- as.data.frame(grid)

# Simulate true occupancy in each landscape using psi ####
# Define function to occupy the landscape
occupy <- function(df){
  occ <- rbinom(nrow(df),size=1,prob=df$psi_t)
  cbind(df,occ)
}
# Occupy the landscape
grid <- occupy(grid)

# Simulate detection histories using true occupancy and probability of detection for K sampling events ####
# Define function to simulate detection histories
makedetections <- function(df){
  det <- NULL
  for (i in 1:K){
    det <- cbind(det, rbinom(dim(df)[1],1,prob = df$occ * df$det.prob))
  }
  det<- as.data.frame(det)
  varname <- "S"  # Select prefix for column names
  names(det)[1:ncol(det)] <- unlist(mapply(function(x,y) paste(x, seq(1,y), sep=""), varname, K)) # Rename columns to S1, S2 etc.
  df <- cbind(df,det) # Recombine detection history with true probabilities 
  # Add site ID numbers
  site <- seq(1:nrow(df))
  df <- cbind(site,df)
}
# Generate detection histories
grid <- makedetections(grid)

# Convert landscape into unmarked dataframe for the model to use ####
# Define function to make unmarked dataframe
makeunmarked <- function(df){
  habitatcov <- df %>% select(X,Z,Q,R)
  det <- df %>% select(starts_with("S",ignore.case = F))
  um_grid <- unmarkedFrameOccu(y=det,siteCovs = habitatcov)
}
# Make unmarked dataframe
um_grid <- makeunmarked(grid)

# Run occupancy model ####
# Define model function
mod_fun <- function(df){
  occu(~ Q + R ~ X + Z, linkPsi="logit",data=df)
}
# Run the model and get the summary
m1 <- mod_fun(um_grid)
summary(m1)


