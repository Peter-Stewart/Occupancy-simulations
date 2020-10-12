# README ####
# In this version effect sizes are fixed at true values
# Purpose of multiple simulations is to check for accurate recovery of true parameter values when explanatory covariates
# and detection histories are randomly generated 


# Packages ####
library(igraph)
library(boot)
library(dplyr)
library(unmarked)
library(beepr)


# Simulation parameters ####
n <- 10000 # Number of times simulation will run
set.seed(135) # Seed for random numbers

# Study design ####
sites <- 267 # Number of sites surveyed
K <- 7 # Number of times each site is surveyed (e.g. no. camera nights)

# Covariate dependence structure ####
fig1 <- graph.formula(X -+ Psi,
                      simplify = FALSE)

# True effect sizes (beta values and intercept) NB. these will be on logit scale ####
# For occupancy
alpha <- 0 # Intercept value for psi (psi in habitat A = 0.50)

# For detection - no covariates in this simulation


#  Empty list to store results of each run ####
results <- list()

# Simulation loop ####
# Run simulation n times
ptm<-proc.time()
for(run in 1:n){
  
  # Random beta coefficient for effect of X on Psi
  B1 <- runif(1, min = -3, max = 3) # Effect of X on Psi (changes psi from 0.05 to 0.95)
  
  # Explanatory variable values ####
  # For occupancy
  X <- rbinom(n=sites,size=1,prob=0.5)
  
  # For detection
 
  # Response variable values other than psi and p ####
 
  
  # True psi value ####
  logit_psi_t <- alpha + (B1*X)
  psi_t <- inv.logit(logit_psi_t)
  
  # True probability of detection (p) ####
  logit.det.prob <-  -2.197225
  det.prob <- inv.logit(logit.det.prob)
  
  
  # Bind Psi and covariates into a "landscape" (dataframe) ####
  grid <- cbind(psi_t, logit_psi_t, X, det.prob, logit.det.prob)
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
    habitatcov <- df %>% select(X)
    det <- df %>% select(starts_with("S",ignore.case = F))
    um_grid <- unmarkedFrameOccu(y=det,siteCovs = habitatcov)
  }
  # Make unmarked dataframe
  um_grid <- makeunmarked(grid)
  
  # Run occupancy model ####
  # Define model function
  mod_fun <- function(df){
    occu(~ 1 ~ X, linkPsi="logit",data=df)
  }
  # Run the model and get the summary
  m1 <- mod_fun(um_grid)
  sum <- summary(m1)

  # Convert summary table into dataframe 
  sumdf <- as.data.frame(t(unlist(sum)))
  colnames(sumdf) <- c("Intercept.est","X.est","Intercept.se","X.se","Intercept.zscore","X.zscore",
                       "Intercept.pval","X.pval",
                       "det.intercept.est",
                       "det.intercept.se",
                       "det.intercept.zscore",
                       "det.intercept.pval")
        
  B1df <- as.data.frame(rep(B1, times=1))
  colnames(B1df) <- "B1"
  sumdf <- cbind(sumdf,B1df)
  
  # Append summary table dataframe to master results 
  run <- paste('run:',run,sep='')
  
  results[[run]] <- sumdf
}

# Turn results list into a dataframe ####
resultsdf <- bind_rows(results, .id = "column_label")

# How long did that take?
new.time<-proc.time()-ptm
beep(sound=3,expr=NULL)
new.time[3]/60

# Add columns for sites and K
resultsdf$sites <- sites
resultsdf$K <- K

# Save results
save(resultsdf,file="C:/Users/PeteS/OneDrive/R Scripts Library/Projects/Durham Occupancy Simulations/Psi_onecov_K7_det01.Rdata")



# Analysis ####
# Is B1 in the 95% confidence interval?
resultsdf$X.ci.up <- resultsdf$X.est + 1.96*resultsdf$X.se
resultsdf$X.ci.lo <- resultsdf$X.est - 1.96*resultsdf$X.se
resultsdf$X.inci <- ifelse(B1 <= resultsdf$X.ci.up,ifelse(resultsdf$X.ci.lo <= B1,1,0),0)

plot(resultsdf$X.inci)


